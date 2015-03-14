" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2015-03-14 14:16:07+09 $
"                   $Revision: 1.67 $
"
" Description:      Please set vimrc the following line if to do
"                   the indentation manually in case labels.
"                   let g:sh_indent_case_labels = 0


if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetShIndent()
setlocal indentkeys+=0=then,0=do,0=else,0=elif,0=fi,0=esac,0=done,0=)
setlocal indentkeys+=0=fin,0=fil,0=fip,0=fir,0=fix
setlocal indentkeys-=:,0#
let s:OutSideQuoteItem = '^\h\w*\s*(\s*)'

if exists("*GetShIndent")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

if !exists("g:sh_indent_case_labels")
  let g:sh_indent_case_labels = 1
endif

function GetShIndent()
  let lnum = prevnonblank(v:lnum - 1)
  if lnum == 0
    return 0
  endif

  if exists("s:tabstop")
    let &tabstop = s:tabstop
    unlet s:tabstop
  endif

  let line = getline(lnum)
  let cline = getline(v:lnum)
  if line =~ '^\s*#' && cline =~ '^\s*$'
    return indent(lnum)
  endif

  let [line, lnum] = s:SkipCommentLine(line, lnum, 0)
  let nnum = lnum
  if cline =~ '^\s*$' && s:InsideQuoteLine(v:lnum)
    return indent(lnum)
  elseif s:InsideQuoteLine(v:lnum)
    return indent(v:lnum)
  endif

  let [snum, hnum, sstr] = s:GetHereDocItem(lnum, line)
  if hnum == lnum
    let [line, lnum] = s:SkipCommentLine(line, snum, 1)
    let [line, lnum] = s:GetHereDocPrevLine(lnum, line)
  elseif hnum >= v:lnum
    let ind = indent(lnum)
    let cind = indent(v:lnum)
    let ind = s:InsideHereDocIndent(snum, hnum, sstr, cline, ind, cind)
    return ind
  elseif hnum < 1 && snum
    return indent(lnum)
  elseif line =~ '\\\@<!"\|\\\@<!\%o47' && s:InsideQuoteLine(lnum)
    let [line, lnum] = s:SkipQuoteLine(line, lnum)
  endif

  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  let ind = indent(lnum)
  let cind = indent(v:lnum)
  let ind = s:MorePrevLineIndent(pline, line, ind)
  let ind = s:InsideCaseLabelIndent(pline, line, ind)
  let ind = s:PrevLineIndent(line, lnum, nnum, pline, cline, ind, cind)
  let ind = s:CurrentLineIndent(cline, ind, cind)

  return ind
endfunction

function s:MorePrevLineIndent(pline, line, ind)
  let ind = a:ind
  if a:pline !~ '\\$' && a:line =~ '\\$'
    let ind = ind + &sw
  elseif a:pline =~ '\\$' && a:line !~ '\\$'
    let ind = ind - &sw
  endif

  return ind
endfunction

function s:InsideCaseLabelIndent(pline, line, ind)
  let ind = a:ind
  if a:line =~ ';;\s*\%(#.*\)\=$'
        \ && a:pline !~# '^\s*case\>' && a:pline !~ ';;\s*\%(#.*\)\=$'
    let ind = ind - &sw
  elseif a:line =~ '^\s*[^(].\{-})' && a:line !~ ';;\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let ind = ind + &sw
  endif

  return ind
endfunction

function s:PrevLineIndent(line, lnum, nnum, pline, cline, ind, cind)
  let ind = a:ind
  if a:line =~ '^\s*[{(]\s*\%(#.*\)\=$'
        \ || a:line =~# '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=$'
        \ || a:line =~ '\%(;\|&&\|||\)\s*\%({\|(\)\s*\%(#.*\)\=$'
    let ind = ind + &sw
  elseif a:line =~# '^\s*case\>' && a:line !~# ';;\s*\<esac\>'
    let ind = s:InsideCaseIndent(ind, a:cline, a:cind)
  elseif a:line =~ '^.\{-}|\%([^|]\|\s*[^#]\)\|^.\{-}[&]\%([^&]\|\s*[^#]\)'
        \ || a:line =~ '^\s*[^(].\{-})' && a:line !~ ';;\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let line = s:HideAnyItemLine(a:line, a:pline)
    for line in split(line, '|\|[&]')
      let ind = s:PrevLineIndent2(line, ind)
      let ind = s:PrevLineIndent3(line, a:lnum, a:nnum, a:pline, ind)
    endfor
  else
    let line = s:HideAnyItemLine(a:line, a:pline)
    let ind = s:PrevLineIndent2(line, ind)
    let ind = s:PrevLineIndent3(line, a:lnum, a:nnum, a:pline, ind)
  endif

  return ind
endfunction

function s:PrevLineIndent2(line, ind)
  let ind = a:ind
  if a:line =~# '^\s*\%(if\|then\|else\|elif\)\>'
        \ && a:line !~# ';\s*\<fi\>'
        \ || a:line =~# '^\s*\%(do\|while\|until\|for\)\>'
        \ && a:line !~# ';\s*\<done\>'
    let ind = ind + &sw
  endif

  return ind
endfunction

function s:PrevLineIndent3(line, lnum, nnum, pline, ind)
  let ind = a:ind
  if a:line =~ '\%(^\s*\|\\\|(\)\@<!)'
        \ && a:pline !~# '^\s*case\>' && a:pline !~ ';;\s*\%(#.*\)\=$'
    let ind = s:ClosedPairIndentPrev(a:nnum, a:line, '(', ')', ind)
  elseif a:line =~ '\$((\|\$(\|\\\@<!(\%(\s*)\)\@!'
    let ind = s:NoClosedPairIndentFore(
          \ a:lnum, a:line, '\$((\|\$(\|\\\@<!(', ind)
  elseif a:line =~ '`'
    let ind = s:ClosedBackQuotePairIndent(a:lnum, a:nnum, a:line, ind)
  endif

  return ind
endfunction

function s:CurrentLineIndent(cline, ind, cind)
  let ind = a:ind
  if a:cline =~# '^\s*case\>' && a:cline !~# ';;\s*\<esac\>'
    call s:GetCaseLabelsIndent(a:cind)
  elseif a:cline =~# '^\s*esac\>'
    let ind = s:CloseEsacIndent(ind)
  elseif a:cline =~# '^\s*\%(then\|do\|else\|elif\|fi\|done\)\>'
        \ || a:cline =~ '^\s*[})]'
    let ind = ind - &sw
  elseif a:cline =~ '^#'
        \ || a:cline =~ '<<[^-]' && a:cind == 0
    let ind = 0
  endif

  return ind
endfunction

function s:InsideHereDocIndent(snum, lnum, sstr, cline, ind, cind)
  let ind = a:ind
  let snum = a:snum
  if snum && a:sstr =~ '<<-\s*\\$' && s:GetNextNonBlank(snum)
    let snum = s:next_lnum
  endif
  unlet! s:next_lnum
  if snum && !&expandtab
    let sind = indent(snum)
  endif
  if a:lnum > 0 && !&expandtab
    let spsum = s:GetMostWidthSpaceLen(a:cline)
  elseif a:lnum > 0 && &expandtab
    let eind = indent(a:lnum)
  endif
  if a:lnum > v:lnum && !&expandtab && spsum >= &sw
    let [tbind, spind] = s:GetTabAndSpaceSum(a:cline, a:cind, a:sstr, sind)
    let s:tabstop = &tabstop
    let &tabstop = spsum + 1
    let ind = tbind * &tabstop + spind
  elseif a:lnum >= v:lnum && !&expandtab && spsum < &sw && a:sstr =~ '<<-'
    let [tbind, spind] = s:GetTabAndSpaceSum(a:cline, a:cind, a:sstr, sind)
    let ind = tbind * &tabstop + spind
  elseif a:lnum >= v:lnum && &expandtab && eind && a:cline =~ '^\t'
    let tbind = matchend(a:cline, '\t*', 0)
    let ind = a:cind - tbind * &tabstop
  elseif a:lnum >= v:lnum
    let ind = a:cind
  endif

  return ind
endfunction

function s:NoClosedPairIndentFore(lnum, line, item1, ind)
  let ind = a:ind
  let icount = 0
  let save_cursor = getpos(".")
  call cursor(a:lnum, 1)
  let snum = search(a:item1, 'cW', a:lnum)
  while snum
    if !s:InsideCommentOrQuote()
      let icount += 1
    endif
    let snum = search(a:item1, 'W', a:lnum)
  endwhile
  call setpos('.', save_cursor)
  if icount
    let ind = ind + &sw * icount
    for line in split(a:line, a:item1)
      let ind = s:PrevLineIndent2(line, ind)
    endfor
  endif

  return ind
endfunction

function s:ClosedPairIndentPrev(lnum, line, item1, item2, ind)
  let ind = a:ind
  let icount = 0
  let save_cursor = getpos(".")
  call cursor(v:lnum, 1)
  while search(a:item2, 'bW', a:lnum)
    if !s:InsideCommentOrQuote()
      let snum = searchpair(a:item1, '', a:item2, 'bW',
            \ 's:InsideCommentOrQuote() || s:InsideHereDoc()')
      if snum > 0 && snum == a:lnum
        continue
      else
        break
      endif
    endif
  endwhile
  if snum > 0 && snum != a:lnum
    while search(a:item1, 'bW', snum)
      if !s:InsideCommentOrQuote()
        let icount += 1
      endif
    endwhile
    let ind = indent(snum)
    if icount && a:item1 =~ '`'
      let ind = ind - &sw * (icount % 2)
    elseif icount
      let ind = ind + &sw * icount
    endif
    let line = getline(snum)
    let [pline, pnum] = s:SkipCommentLine(line, snum, 1)
    let ind = s:MorePrevLineIndent(pline, a:line, ind)
    let ind = s:InsideCaseLabelIndent(pline, a:line, ind)
    let ind = s:PrevLineIndent2(line, ind)
  endif
  call setpos('.', save_cursor)

  return ind
endfunction

function s:ClosedBackQuotePairIndent(lnum, nnum, line, ind)
  let ind = a:ind
  let line = s:GetHideInsideQuoteJoinLine(a:nnum)
  let item = s:GetBackQuoteItem(line)

  if a:line !~ item . ".*" . item && len(split(line, item, 1)) % 2
    let ind = s:ClosedPairIndentPrev(a:nnum, a:line, item, item, ind)
  elseif a:line !~ item . ".*" . item
    let ind = s:NoClosedPairIndentFore(a:lnum, a:line, item, ind)
  endif

  return ind
endfunction

function s:GetHideInsideQuoteJoinLine(lnum)
  let snum = search(s:OutSideQuoteItem, 'nbW')
  let snum = snum ? snum : 1
  let line = ""
  let qinit = 0
  while snum <= a:lnum
    let nline = getline(snum)
    if !qinit && nline =~ '^\s*#'
      let snum += 1
      continue
    elseif !qinit && nline =~ '"\|\%o47'
      let [nline, qinit] = s:GetInitQuote(nline, 1, 1)
    elseif !qinit && nline =~ '#'
      let nline = substitute(
            \ nline, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
      let nline = substitute(nline, '^\(.*\)#.*$', '\1', '')
    elseif !qinit && nline =~ '<<-\='
      let lnum = s:SkipHereDocLineFore(snum)
      if lnum < 1
        break
      else
        let snum = lnum + 1
        continue
      endif
    elseif qinit && nline !~ '"\|\%o47'
      let snum += 1
      continue
    endif
    let line = line . nline

    if qinit == 2 && nline =~ '\\\@<!"'
          \ && len(split(line, '\\\@<!"', 1)) % 2
      let [line, qinit] = s:GetInitQuote(line, 1, 1)
      let qinit = 0
    elseif qinit == 1 && nline =~ '\\\@<!\%o47'
          \ && len(split(line, '\\\@<!\%o47', 1)) % 2
      let [line, qinit] = s:GetInitQuote(line, 1, 1)
      let qinit = 0
    endif
    let snum += 1
  endwhile

  return line
endfunction

function s:GetBackQuoteItem(line)
  let item = 0
  let sum = matchend(a:line, '^.*`') - 2
  while sum
    let str = strpart(a:line, sum, 1)
    if str != '\'
      break
    else
      let item += 1
    endif
    let sum -= 1
  endwhile
  let item = '\\\@<!\\\{' . item . '}`'

  return item
endfunction

function s:SkipHereDocLineFore(snum)
  let save_cursor = getpos(".")
  call cursor(a:snum, 1)
  call search('<<-\=', 'W', a:snum)
  let [sstr, estr, dummy] = s:GetHereDocPairLine(a:snum)
  let lnum = searchpair(sstr, '', estr, 'nW')
  call setpos('.', save_cursor)

  return lnum
endfunction

function s:CloseEsacIndent(ind)
  let ind = a:ind
  if g:sh_indent_case_labels
    let ind = ind - &sw
  else
    let save_cursor = getpos(".")
    call cursor(v:lnum, 1)
    let lnum = searchpair('\C^\s*case\>', '', '\C^\s*esac\>', 'bW',
          \ 's:InsideCommentOrQuote() || s:InsideHereDoc()')
    call setpos('.', save_cursor)
    if lnum > 0
      let ind = indent(lnum)
    endif
  endif

  return ind
endfunction

function s:SkipCommentLine(line, lnum, prev)
  let line = a:line
  let lnum = a:lnum
  while line =~ '^\s*#' && s:GetPrevNonBlank(lnum)
    let lnum = s:prev_lnum
    let line = getline(lnum)
  endwhile
  if a:prev && s:GetPrevNonBlank(lnum)
    let lnum = s:prev_lnum
    let line = getline(lnum)
    while line =~ '^\s*#' && s:GetPrevNonBlank(lnum)
      let lnum = s:prev_lnum
      let line = getline(lnum)
    endwhile
  elseif a:prev
    let lnum = 0
    let line = ""
  endif
  unlet! s:prev_lnum

  return [line, lnum]
endfunction

function s:GetPrevNonBlank(lnum)
  let s:prev_lnum = prevnonblank(a:lnum - 1)

  return s:prev_lnum
endfunction

function s:GetNextNonBlank(lnum)
  let s:next_lnum = nextnonblank(a:lnum + 1)

  return s:next_lnum
endfunction

function s:InsideCaseIndent(ind, cline, cind)
  let ind = a:ind
  if g:sh_indent_case_labels
    let ind = ind + &sw
  elseif !g:sh_indent_case_labels && !exists("s:case_labels_ind")
    let ind = a:cind
  elseif exists("s:case_labels_ind") && s:case_labels_ind
    let ind = ind + s:case_labels_ind
  endif
  if exists("s:case_labels_ind") && a:cline !~ '^\s*#'
    unlet s:case_labels_ind
  endif

  return ind
endfunction

function s:GetCaseLabelsIndent(cind)
  if !g:sh_indent_case_labels && s:GetNextNonBlank(v:lnum)
    let clind = indent(s:next_lnum)
    if clind != a:cind && clind - a:cind > -1
      let s:case_labels_ind = clind - a:cind
    endif
  endif
  unlet! s:next_lnum
endfunction

function s:GetHereDocPrevLine(lnum, line)
  let lnum = a:lnum
  let line = a:line
  while 1
    let [snum, hnum, sstr] = s:GetHereDocItem(lnum)
    if hnum == lnum
      let [line, lnum] = s:SkipCommentLine(line, snum, 1)
    else
      break
    endif
  endwhile
  if line =~ '\\\@<!"\|\\\@<!\%o47' && s:InsideQuoteLine(lnum)
    let [line, lnum] = s:SkipQuoteLine(line, lnum)
  endif

  return [line, lnum]
endfunction

function s:GetHereDocPairLine(lnum)
  let line = getline(a:lnum)
  if line =~ '<<-\=\s*\\$' && s:GetNextNonBlank(a:lnum)
    let estr = getline(s:next_lnum)
    let estr = substitute(estr, '^\s*', '', '')
  elseif line =~ '<<-\=\s*\\$'
    let estr = ""
  else
    let estr = substitute(line, '^.*<<-\=\s*', '', '')
  endif
  unlet! s:next_lnum
  if estr =~ '^"\|\%o47'
    let estr = substitute(estr, '^\%("\|\%o47\)\(\S\+\)\%("\|\%o47\)', '\1', '')
  else
    let estr = substitute(estr, '^\\', '', '')
    let estr = substitute(estr, '\s*\%(\\\@<!|\|\\\@<!>\).*$', '', '')
  endif
  if line =~ '<<-'
    let sstr = '\C\%(^\s*#.*\)\@<!<<-\s*\%(\\\n\s*\)\=\%("\|\%o47\|\\\)\=\M' .
          \ estr . '\m\%("\|\%o47\)\='
    let estr = '\C\%(<<-\=\s*\\\n\)\@<!\_^\t*\M' . estr . '\m$'
  else
    let sstr = '\C\%(^\s*#.*\)\@<!<<\s*\%(\\\n\s*\)\=\%("\|\%o47\|\\\)\=\M' .
          \ estr . '\m\%("\|\%o47\)\='
    let estr = '\C\%(<<-\=\s*\\\n\)\@<!\_^\M' . estr . '\m$'
  endif

  return [sstr, estr, line]
endfunction

function s:GetHereDocItem(lnum, ...)
  let snum = 0
  let lnum = 0
  let onum = 0
  let line = ""
  let save_cursor = getpos(".")
  call cursor(a:lnum, a:0 ? len(a:1) : 1)
  while search('\%(^\s*#.*\)\@<!<<-\=\s*\S\+', 'bW')
    let snum = line(".")
    let [sstr, estr, line] = s:GetHereDocPairLine(snum)
    let lnum = searchpair(sstr, '', estr, 'nW')
    if lnum >= a:lnum || lnum < 1 || lnum < onum
      break
    endif
    let onum = lnum
  endwhile
  call setpos('.', save_cursor)

  return [snum, lnum, line]
endfunction

function s:GetMostWidthSpaceLen(line)
  let idx = 0
  let spsum = 0
  while 1
    let idx = stridx(a:line, ' ', idx)
    if idx < 0
      break
    endif
    let len = matchend(a:line, ' *', idx) - idx
    if len > spsum
      let spsum = len
    endif
    let idx = idx + len
  endwhile

  return spsum
endfunction

function s:GetTabAndSpaceSum(cline, cind, sstr, sind)
  if a:cline =~ '^\t'
    let tbind = matchend(a:cline, '\t*', 0)
  else
    let tbind = 0
  endif
  let spind = a:cind - tbind * &sw
  if a:sstr =~ '<<-' && a:sind
    let tbind = a:sind / &sw
  endif

  return [tbind, spind]
endfunction

function s:InsideQuoteLine(lnum)
  let init = 0
  let line = ""
  let snum = search(s:OutSideQuoteItem, 'nbW')
  let snum = snum ? snum : 1

  while snum < a:lnum
    let nline = getline(snum)
    if !init && nline !~ '\\\@<!"' && nline !~ '\\\@<!\%o47'
          \ || nline =~ '^\s*#' && !init
      let snum += 1
      continue
    elseif !init && nline =~ '#'
      let nline = substitute(
            \ nline, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
      let nline = substitute(nline, '^\(.*\)#.*$', '\1', '')
    elseif !init && nline =~ '"\|\%o47'
      let nline = substitute(
            \ nline, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
    elseif init
      let line = line . nline
    endif

    if !init && nline =~ '\\\@<!"\|\\\@<!\%o47'
      let [hnum, lnum, sstr] = s:GetHereDocItem(snum)
      if lnum > snum
        let snum = lnum
      elseif lnum < 1 && hnum
        break
      else
        let [line, init] = s:GetInitQuote(nline, 0, 1)
      endif
    elseif init == 2
          \ && nline =~ '\\\@<!"' && len(split(line, '\\\@<!"', 1)) % 2
      let [line, init] = s:GetInitQuote(line, 1, 1)
    elseif init == 1
          \ && nline =~ '\\\@<!\%o47' && len(split(line, '\\\@<!\%o47', 1)) % 2
      let [line, init] = s:GetInitQuote(line, 1, 1)
    endif
    let snum += 1
  endwhile

  return init
endfunction

function s:GetInitQuote(line, wipe, lret)
  let line = a:line
  if a:wipe
    let line = substitute(
          \ line, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
    let line = substitute(line, '^\(.*\)#.*$', '\1', '')
  endif
  let dq = match(line, '\\\@<!"')
  let sq = match(line, '\\\@<!\%o47')
  if dq > -1 && sq < 0 || dq > -1 && dq < sq
    let init = 2
  elseif sq > -1 && dq < 0 || sq > -1 && sq < dq
    let init = 1
  else
    let init = 0
  endif

  if a:lret
    return [line, init]
  else
    return init
  endif
endfunction

function s:HideAnyItemLine(line, pline)
  let line = substitute(
        \ a:line, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
  let line = substitute(line, '#.*$', '', '')
  let line = substitute(
        \ line, '\$\=([^()]*\\\@<!)\|\(\\\@<!\\*`\).*\\\@<!\1', '', 'g')
  let len = len(line)
  while 1
    let line = substitute(line, '\$\=([^()]*\\\@<!)', '', 'g')
    if len == len(line)
      break
    else
      let len = len(line)
    endif
  endwhile
  if a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$'
    let line = substitute(line, '^\s*.\{-})', '', '')
  endif

  return line
endfunction

function s:SkipQuoteLine(line, lnum)
  let lnum = a:lnum
  while s:InsideQuoteLine(lnum) && s:GetPrevNonBlank(lnum)
    let lnum = s:prev_lnum
  endwhile
  unlet! s:prev_lnum
  let line = getline(lnum)
  let line = substitute(
        \ line, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
  let line = line . "|" .
        \ substitute(a:line, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
  let line = substitute(
        \ line, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')

  return [line, lnum]
endfunction

function s:InsideCommentOrQuote()
  let lnum = line(".")
  let line = getline(lnum)
  let cnum = col(".")
  let sum = match(line, '\S')
  let squote = 0
  let dquote = 0
  let init = s:InsideQuoteLine(lnum)
  while sum < cnum
    let str = strpart(line, sum, 1)
    if str == '#' && !squote && !dquote
      return 1
    elseif str == "'" && !squote && !dquote
      let squote = 1
    elseif str == "'" && squote
      let squote = 0
    elseif str == '"' && !squote && !dquote
      let dquote = 1
    elseif str == '"' && dquote && laststr != '\'
      let dquote = 0
    endif
    let laststr = str
    let sum += 1
  endwhile

  if (squote || dquote) && !init || !squote && !dquote && init
    return 1
  else
    return 0
  endif
endfunction

function s:InsideHereDoc()
  let lnum = line(".")
  let [snum, hnum, sstr] = s:GetHereDocItem(lnum)

  if hnum >= lnum || hnum < 1 && snum
    return 1
  else
    return 0
  endif
endfunction

autocmd InsertEnter <buffer> call <SID>UnletVariables()
function s:UnletVariables()
  if exists("s:tabstop")
    let &tabstop = s:tabstop
    unlet s:tabstop
  endif
  if exists("s:case_labels_ind")
    unlet s:case_labels_ind
  endif
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: set sts=2 sw=2 expandtab smarttab:
