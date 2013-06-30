" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2013-06-30 18:09:17+09 $
"                   $Revision: 1.42 $
"
" Description:      Please set vimrc the following line if to do
"                   the indentation manually in case labels.
"                   let g:sh_indent_case_labels = 0


if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetShIndent()
setlocal indentkeys+=0=then,0=do,0=else,0=elif,0=fi,0=esac,0=done
setlocal indentkeys+=0=fin,0=fil,0=fip,0=fir,0=fix
setlocal indentkeys-=:,0#

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
  if !&ai && cline =~ '^$' && s:InsideQuote(v:lnum)
    return indent(lnum)
  elseif s:InsideQuote(v:lnum)
    return indent(v:lnum)
  elseif s:InsideQuote(lnum) && !s:InsideQuote(v:lnum)
    let [line, lnum] = s:SkipQuoteLine(line, lnum)
  endif

  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  if s:InsideQuote(pnum) && !s:InsideQuote(lnum)
    let [pline, pnum] = s:SkipQuoteLine(pline, pnum)
  endif

  let cind = indent(v:lnum)
  let ind = indent(lnum)
  let ind = s:MorePrevLineIndent(pline, line, ind)
  let ind = s:PrevLineIndent(line, lnum, pline, cline, ind)
  let ind = s:CurrentLineIndent(cline, ind, cind)
  let ind = s:InsideHereDocIndent(cline, lnum, ind, cind)

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

function s:PrevLineIndent(line, lnum, pline, cline, ind)
  let ind = a:ind
  if a:line =~# '^\s*\%(if\|then\|else\|elif\)\>' && a:line !~# ';\s*\<fi\>'
        \ || (a:line =~# '^\s*\%(do\|while\|until\|for\)\>'
        \ || a:line =~# '\%(\n.*\)\@<!\%(|\|;\)\s*\%(while\|until\)\>'
        \ || a:line =~# '\%(\n.*\)\@<!\<for\>\s\+\h\w*\s\+in\>')
        \ && a:line !~# ';\s*\<done\>'
        \ || a:line =~ '^\s*{\s*\%(#.*\)\=$'
        \ || a:line =~ '^\s*{\s*\%(#.*\)\=\n'
        \ || a:line =~ '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=$'
        \ || a:line =~ '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=\n'
        \ || a:line =~ '\%(\n.*\)\@<!\%(&&\|||\)\s*{\s*\%(#.*\)\=$'
        \ || (a:line =~ '^\s*[^(].*)\s*\%(#.*\)\=$'
        \ || a:line =~ '^\s*[^(].*)\s*\%(#.*\)\=\n')
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let ind = ind + &sw
  elseif a:line =~# '^\s*case\>' && a:line !~# ';;\s*\<esac\>'
    let ind = s:InsideCaseIndent(ind, a:cline)
  elseif a:line =~ '\%(\n.*\)\@<!;;\s*\%(#.*\)\=$' && a:line !~ '^\s*[^(].*)'
    let ind = ind - &sw
  elseif (a:line =~ '^\t*\S\+$' || a:line =~ '^\t*\S\+\n')
        \ && a:line !~ '^\t*}' && a:line !~ '^\h\w*()'
        \ && a:line !~# '\<\%(fi\|done\|esac\|echo\|shift\|continue' .
        \ '\|break\|exit\|return\)\>'
    let [sstr, estr] = s:GetHereDocPairLine1(a:line)
    let ind = s:ClosePairIndent(sstr, estr, a:lnum, a:cline, ind, 1)
  endif
  if a:line =~ '\n'
    let line = substitute(a:line, '^.*\n', '', '')
    let ind = s:PrevLineIndent(line, a:lnum, a:pline, a:cline, ind)
  endif

  return ind
endfunction

function s:CurrentLineIndent(cline, ind, cind)
  let ind = a:ind
  if a:cline =~# '^\s*case\>' && a:cline !~# ';;\s*\<esac\>'
        \ && exists("g:sh_indent_case_labels") && !g:sh_indent_case_labels
    let clind = indent(nextnonblank(v:lnum + 1))
    if clind > -1 && clind != a:cind && clind - a:cind > -1
      let s:case_labels_ind = clind - a:cind
    endif
  elseif a:cline =~# '^\s*fi\>' && ind != a:cind
    let ind = s:ClosePairIndent(
          \ '\C^\s*if\>', '\C^\s*fi\>', v:lnum, a:cline, ind, 0)
  elseif a:cline =~# '^\s*esac\>'
        \ && exists("g:sh_indent_case_labels") && !g:sh_indent_case_labels
    let ind = s:ClosePairIndent(
          \ '\C^\s*case\>', '\C^\s*esac\>', v:lnum, a:cline, ind, 0)
  elseif a:cline =~# '^\s*\%(then\|do\|else\|elif\|fi\|done\|esac\)\>'
        \ || a:cline =~ '^\s*}'
    let ind = ind - &sw
  elseif a:cline =~ '^#'
        \ || a:cline =~ '<<[^-]' && a:cind == 0
        \ || a:cline =~ '\\\@<!"\|\\\@<!\%o47' && a:cind == 0
        \ && get(s:GetInitQuote(a:cline, 1), 1)
    let ind = 0
  endif

  return ind
endfunction

function s:InsideHereDocIndent(cline, lnum, ind, cind)
  let ind = a:ind
  let [snum, lnum, sstr] = s:InsideHereDoc(v:lnum)
  if snum && !&expandtab
    let sind = indent(snum)
  endif
  if lnum > 0 && !&expandtab
    let spsum = s:GetMostWidthSpaceLen(a:cline)
  elseif lnum > 0 && &expandtab
    let eind = indent(lnum)
  endif
  if lnum > v:lnum && !&expandtab && spsum >= &sw
    let [tbind, spind] = s:GetTabAndSpaceSum(a:cline, a:cind, sstr, sind)
    let s:tabstop = &tabstop
    let &tabstop = spsum + 1
    let ind = tbind * &tabstop + spind
  elseif lnum >= v:lnum && !&expandtab && spsum < &sw && sstr =~ '<<-'
    let [tbind, spind] = s:GetTabAndSpaceSum(a:cline, a:cind, sstr, sind)
    let ind = tbind * &tabstop + spind
  elseif lnum >= v:lnum && &expandtab && eind && a:cline =~ '^\t'
    let tbind = matchend(a:cline, '\t*', 0)
    let ind = a:cind - tbind * &tabstop
  elseif lnum >= v:lnum
    let ind = a:cind
  elseif lnum < 1 && snum
    let ind = indent(a:lnum)
  endif

  return ind
endfunction

function s:ClosePairIndent(sstr, estr, lnum, cline, ind, prev)
  let ind = a:ind
  let save_cursor = getpos(".")
  call cursor(a:lnum, 1)
  let lnum = searchpair(a:sstr, '', a:estr, 'bW', 's:InsideQuote(line("."))')
  call setpos('.', save_cursor)
  if lnum > 0 && a:prev
    let line = getline(lnum)
    let [line, lnum] = s:SkipCommentLine(line, lnum, 1)
    if s:InsideQuote(lnum) && !s:InsideQuote(v:lnum)
      let [line, lnum] = s:SkipQuoteLine(line, lnum)
    endif
    let [pline, pnum] = s:SkipCommentLine(line, lnum ,1)
    let ind = indent(lnum)
    let ind = s:MorePrevLineIndent(pline, line, ind)
    let ind = s:PrevLineIndent(line, lnum, pline, a:cline, ind)
  elseif lnum > 0
    let ind = indent(lnum)
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
  endif
  unlet s:prev_lnum

  return [line, lnum]
endfunction

function s:GetPrevNonBlank(lnum)
  let s:prev_lnum = prevnonblank(a:lnum - 1)

  return s:prev_lnum
endfunction

function s:InsideCaseIndent(ind, cline)
  let ind = a:ind
  if exists("g:sh_indent_case_labels") && g:sh_indent_case_labels
    let ind = ind + &sw
  elseif exists("s:case_labels_ind") && s:case_labels_ind
    let ind = ind + s:case_labels_ind
  endif
  if exists("s:case_labels_ind") && a:cline !~ '^\s*#'
    unlet s:case_labels_ind
  endif

  return ind
endfunction

function s:GetHereDocPairLine1(line)
  let line = a:line
  if line =~ '\n'
    let line = substitute(line, '\n.*$', '', '')
  endif
  if line =~ '^\t\+'
    let sstr = matchstr(line, '\S\+$')
  else
    let sstr = line
  endif
  let sstr = '\C<<-\=\s*\%("\|\%o47\|\\\)\=\M' . sstr . '\m\%("\|\%o47\)\='
  let estr = '\C^\M' . line . '\m$'

  return [sstr, estr]
endfunction

function s:GetHereDocPairLine2()
  let line = getline(".")
  let estr = substitute(line, '^.*<<-\=\s*', '', '')
  if estr =~ '^"\|\%o47'
    let estr = substitute(estr, '^\%("\|\%o47\)\(\S\+\)\%("\|\%o47\)', '\1', '')
  else
    let estr = substitute(estr, '^\\', '', '')
    let estr = substitute(estr, '\s*\%(\\\@<!|\|\\\@<!>\).*$', '', '')
  endif
  if line =~ '<<-'
    let sstr = '\C<<-\s*\%("\|\%o47\|\\\)\=\M' . estr . '\m\%("\|\%o47\)\='
    let estr = '\C^\t*\M' . estr . '\m$'
  else
    let sstr = '\C<<\s*\%("\|\%o47\|\\\)\=\M' . estr . '\m\%("\|\%o47\)\='
    let estr = '\C^\M' . estr . '\m$'
  endif

  return [sstr, estr]
endfunction

function s:InsideHereDoc(lnum)
  let snum = 0
  let lnum = 0
  let onum = 0
  let sstr = ""
  let save_cursor = getpos(".")
  call cursor(a:lnum, 1)
  while search('<<-\=\s*\S\+', 'bW')
    let snum = line(".")
    let [sstr, estr] = s:GetHereDocPairLine2()
    let lnum = searchpair(sstr, '', estr, 'nW')
    if lnum >= a:lnum || lnum < 1 || lnum < onum
      break
    endif
    let onum = lnum
  endwhile
  call setpos('.', save_cursor)

  return [snum, lnum, sstr]
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

function s:InsideQuote(lnum)
  let init = 0
  let line = ""
  let snum = s:GetOutSideQuote()

  let snum += 1
  while snum < a:lnum
    let nline = getline(snum)
    let snum += 1
    if nline !~ '\\\@<!"' && nline !~ '\\\@<!\%o47' && !init
      continue
    elseif nline =~ '^\s*#' && !init
      continue
    elseif nline =~ '#' && !init
      let nline = substitute(
            \ nline, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
      let nline = substitute(nline, '^\(.*\)#.*$', '\1', '')
    elseif nline =~ '"\|\%o47' && !init
      let nline = substitute(
            \ nline, "'[^']*'" . '\|"\%([^"]\|\\"\)*\\\@<!"', '', 'g')
    endif
    if init
      let line = line . nline
    endif

    if !init && nline =~ '\\\@<!"\|\\\@<!\%o47'
      let [hnum, lnum, sstr] = s:InsideHereDoc(snum - 1)
      if lnum >= snum - 1
        let snum = lnum + 1
      elseif lnum < 1 && hnum
        break
      else
        let [line, init] = s:GetInitQuote(nline, 0)
      endif
    elseif init == 2
          \ && nline =~ '\\\@<!"' && len(split(line, '\\\@<!"', 1)) % 2
      let [line, init] = s:GetInitQuote(line, 1)
    elseif init == 1
          \ && nline =~ '\\\@<!\%o47' && len(split(line, '\\\@<!\%o47', 1)) % 2
      let [line, init] = s:GetInitQuote(line, 1)
    endif
  endwhile

  return init
endfunction

function s:GetOutSideQuote()
  let snum = 0
  let slist = [search('^\h\w*\s*(\s*)\s*{\=\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*fi\>\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*esac\>\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*done\>\s*\%(#.*\)\=$', 'nbW')]
  for enum in slist
    if enum > snum
      let snum = enum
    endif
  endfor

  return snum
endfunction

function s:GetInitQuote(line, wipe)
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

  return [line, init]
endfunction

function s:SkipQuoteLine(line, lnum)
  let lnum = a:lnum
  while s:InsideQuote(lnum)
    let lnum = prevnonblank(lnum - 1)
  endwhile
  let line = getline(lnum)
  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  if pnum != lnum && s:InsideQuote(pnum)
    let [line, lnum] = s:SkipQuoteLine(pline, pnum)
  endif
  if indent(lnum) == 0
    let [line, lnum] = s:SkipCommentLine(line, lnum, 1)
  endif
  let line = line . "\n" . a:line

  return [line, lnum]
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
