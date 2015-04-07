" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2015-04-07 17:35:41+09 $
"                   $Revision: 3.8 $
"
" Description:      Specifies a string search quoting lines from the variable
"                   g:sh_indent_outside_quote_item. You can use regular
"                   expressions can be used by Vim. If the unlet or string
"                   empty to reset. The default value is '^\h\w*\s*(\s*)'
"
"                   Set the following line if you do not use a mechanism to
"                   turn off Auto-indent in single quoting lines. Works even
"                   without the variable g:sh_indent_outside_quote_item.
"                   let g:sh_indent_auto_off = 0


if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GetShIndent()
setlocal indentkeys+=0=then,0=do,0=else,0=elif,0=fi,0=esac,0=done,0=)
setlocal indentkeys+=0=fin,0=fil,0=fip,0=fir,0=fix
setlocal indentkeys-=:,0#

if exists("*GetShIndent")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

let s:OutSideQuoteItem = '^\h\w*\s*(\s*)'
if !exists("g:sh_indent_outside_quote_item")
  let g:sh_indent_outside_quote_item = s:OutSideQuoteItem
endif

if !exists("g:sh_indent_auto_off")
  let g:sh_indent_auto_off = 1
endif

function GetShIndent()
  let lnum = prevnonblank(v:lnum - 1)
  if lnum == 0
    return 0
  endif

  if exists("b:sh_indent_tabstop")
    let &tabstop = b:sh_indent_tabstop
    unlet b:sh_indent_tabstop
  endif

  let line = getline(lnum)
  let cline = getline(v:lnum)
  if g:sh_indent_auto_off
    if !exists("g:sh_indent_outside_quote_item")
          \ || !len(g:sh_indent_outside_quote_item)
      let g:sh_indent_outside_quote_item = s:OutSideQuoteItem
    endif

    let qinitdic = s:GetJoinLineAndQuoteInit(lnum)
    if cline =~ '^\s*$' && qinitdic[lnum] % 2
      return indent(lnum)
    elseif qinitdic[lnum] % 2
      return indent(v:lnum)
    endif
  endif

  while 1
    let [snum, hnum, sstr] = s:GetHereDocItem(lnum, line)
    if hnum == lnum
      let [line, lnum] = [sstr, snum]
      if line =~ '\\$'
        let line = substitute(line, '\\$', '', '')
      endif
      break
    elseif hnum >= v:lnum
      let ind = indent(lnum)
      let cind = indent(v:lnum)
      let ind = s:InsideHereDocIndent(snum, hnum, sstr, cline, ind, cind)
      return ind
    else
      if g:sh_indent_auto_off
            \ && line =~ '\\\@<!"\|\\\@<!\%o47' && qinitdic[lnum - 1]
        let [line, lnum] = s:SkipQuoteLine(line, lnum - 1, qinitdic)
        break
      elseif line =~ '^\s*#' && s:GetPrevNonBlank(lnum)
        let lnum = s:prev_lnum
        let line = getline(lnum)
        unlet s:prev_lnum
        continue
      else
        unlet! s:prev_lnum
        break
      endif
    endif
  endwhile

  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  let ind = indent(lnum)
  let ind = s:MorePrevLineIndent(pline, line, ind)
  let ind = s:InsideCaseLabelIndent(pline, line, ind)
  if g:sh_indent_auto_off
    let ind = s:PrevLineIndent(line, lnum, pline, ind, qinitdic)
  else
    let ind = s:PrevLineIndent(line, lnum, pline, ind)
  endif
  let ind = s:CurrentLineIndent(cline, ind)

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
  if a:line =~# '^\s*esac\>\s*;;\s*\%(#.*\)\=$'
        \ && a:pline =~ ';;\s*\%(#.*\)\=$'
    let ind = ind - &sw
  elseif a:line =~ ';;\s*\%(#.*\)\=$'
        \ && a:pline !~# '^\s*case\>\|^\s*[^(].\{-})\s*case\>'
        \ && a:pline !~ ';;\s*\%(#.*\)\=$'
    let ind = ind - &sw
  elseif a:line =~ '^\s*[^(].\{-})' && a:line !~ ';;\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>\|^\s*[^(].\{-})\s*case\>'
        \ || a:pline =~ ';;\s*\%(#.*\)\=$')
    let ind = ind + &sw
  endif

  return ind
endfunction

function s:PrevLineIndent(line, lnum, pline, ind, ...)
  let ind = a:ind
  if a:line =~ '^\s*[{(]\s*\%(#.*\)\=$'
        \ || a:line =~ '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=$'
        \ || a:line =~ '\%(;\|&&\|||\)\s*\%({\|(\)\s*\%(#.*\)\=$'
    let ind = ind + &sw
  elseif a:line =~ '|\|&\|`\|('
        \ || a:line =~ '^\s*[^(].\{-})' && a:line !~ ';;\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let line = s:HideAnyItemLine(a:line, a:lnum, a:pline)
    if line =~ '\$('
      let ind = ind + &sw * (len(split(line, '\$(', 1)) - 1)
    endif
    if a:0 && line =~ '`' && len(split(a:1['line'], '`', 1)) % 2 == 0
      let ind = ind + &sw
    elseif a:0 && line =~ '`'
      let ind = ind - &sw
    endif
    for line in split(line, '|\|&\|`\|(')
      if len(line)
        let ind = s:PrevLineIndent2(line, ind)
      endif
    endfor
  else
    let line = s:HideAnyItemLine(a:line, a:lnum, a:pline)
    if line =~ '\%(^\s*\|(\|\\\)\@<!)'
      let ind = ind - &sw * (len(split(line, ')', 1)) - 1)
    endif
    let ind = s:PrevLineIndent2(line, ind)
  endif

  return ind
endfunction

function s:PrevLineIndent2(line, ind)
  let ind = a:ind
  if a:line =~# '^\s*\%(if\|then\|else\|elif\)\>'
        \ && a:line !~# ';\s*\<fi\>'
        \ || a:line =~# '^\s*\%(do\|while\|until\|for\)\>'
        \ && a:line !~# ';\s*\<done\>'
        \ || a:line =~# '^\s*case\>'
        \ && a:line !~# ';;\s*\<esac\>'
    let ind = ind + &sw
  endif

  return ind
endfunction

function s:CurrentLineIndent(cline, ind)
  let ind = a:ind
  if a:cline =~# '^\s*\%(then\|do\|else\|elif\|fi\|done\|esac\)\>'
        \ || a:cline =~ '^\s*[})]'
    let ind = ind - &sw
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

function s:HideAnyItemLine(line, lnum, pline)
  let line = a:line
  if line =~ '\%(\${\h\w*#\=\|\${\=\|\\\)\@<!#'
    let sum = s:InsideComment(a:lnum)
    if sum
      let line = substitute(line, '^\(.\{' . sum . '}\)#.*$', '\1', '')
    endif
    let line = substitute(
          \ line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
  elseif line =~ "'"
    let line = substitute(line, "'[^']*'", '', 'g')
  endif
  let line = substitute(
        \ line, '\$\=([^()]*\\\@<!)\|\(\\\@<!\\*`\).\{-}\\\@<!\1', '', 'g')
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

function s:GetHereDocPairLine(line, lnum)
  if a:line =~ '<<-\=\s*\\$' && s:GetNextNonBlank(a:lnum)
    let estr = getline(s:next_lnum)
    let estr = substitute(estr, '^\s*', '', '')
  elseif a:line =~ '<<-\=\s*\\$'
    let estr = ""
  else
    let estr = substitute(a:line, '^.\{-}<<-\=\s*\\\=', '', '')
  endif
  unlet! s:next_lnum
  if estr =~ "^'"
    let estr = substitute(estr, '^\%o47\([^' . "'" . ']\+\)\%o47.*$', '\1', '')
  elseif estr =~ '^"'
    let estr = substitute(estr, '^"\([^"]\+\)".*$', '\1', '')
  else
    let estr = substitute(estr, '\s*\%(\\\@<!|\|\\\@<!>\|\\\@<!#\).*$', '', '')
  endif
  if len(estr) && a:line =~ '<<-'
    let estr = '\C\%(<<-\=\s*\\\n\)\@<!\_^\t*\M' . estr . '\m$'
  elseif len(estr)
    let estr = '\C\%(<<\s*\\\n\)\@<!\_^\M' . estr . '\m$'
  endif

  return estr
endfunction

function s:OnOrNotItem(line, item)
  let line = substitute(
        \ a:line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
  let line = substitute(line, '\%(\${\h\w*#\=\|\${\=\|\\\)\@<!#.*$', '', '')

  if line =~# a:item
    return 1
  else
    return 0
  endif
endfunction

function s:GetHereDocItem(lnum, ...)
  let snum = 0
  let lnum = 0
  let onum = 0
  let line = ""
  let save_cursor = getpos(".")
  call cursor(a:lnum, a:0 ? len(a:1) : 1)
  while search('\%(^\s*#.\{-}\)\@<!<<-\=\s*\S\+', 'bW')
    let lsum = line(".")
    let sstr = getline(lsum)
    if s:OnOrNotItem(sstr, '<<')
      let estr = s:GetHereDocPairLine(sstr, lsum)
    else
      continue
    endif
    if !len(estr)
      continue
    endif
    let snum = lsum
    let lnum = search(estr, 'nW')
    let line = sstr
    if lnum < onum
      let lnum = onum
      let snum = pnum
      let line = pline
      break
    elseif lnum > a:lnum
      break
    elseif snum && lnum < 1
      continue
    endif
    let onum = lnum
    let pnum = snum
    let pline = line
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
    let b:sh_indent_tabstop = &tabstop
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

function s:GetJoinLineAndQuoteInit(lnum)
  let snum = search(g:sh_indent_outside_quote_item, 'nbW')
  let snum = snum ? snum : 1
  let qinit = 0
  let line = ""
  let qinitdic = {}
  while snum <= a:lnum
    let nline = getline(snum)
    if !qinit && nline =~ '^\s*#'
      let qinitdic[snum] = qinit
      let snum += 1
      continue
    elseif !qinit && nline =~ '<<-\=' && s:OnOrNotItem(nline, '<<')
      let hlnum = get(s:GetHereDocItem(snum, nline), 1)
      if hlnum > snum
        let snum = hlnum
      endif
    elseif !qinit && nline =~ '\%(\${\h\w*#\=\|\${\=\|\\\)\@<!#'
      let sum = s:InsideComment(snum)
      if sum
        let nline = substitute(nline, '^\(.\{' . sum . '}\)#.*$', '\1', '')
      endif
      let nline = substitute(
            \ nline, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
    elseif !qinit && nline =~ '\%o47'
      let nline = substitute(nline, "'[^']*'", '', 'g')
    endif
    if nline =~ '"\|\%o47\|`'
      let line = line . nline
    endif

    if !qinit && nline =~ '\\\@<!"\|\\\@<!\%o47'
      let qinit = s:GetInitQuote(nline)
    endif
    if qinit == 2 && len(split(line, '\\\@<!\%o47', 1)) % 2 == 0
      let qinit = 3
    elseif qinit == 3 && len(split(line, '\\\@<!\%o47', 1)) % 2
          \ && len(split(line, '\\\@<!"', 1)) % 2
      let line = substitute(
            \ line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
      let qinit = 0
    elseif qinit == 3 && len(split(line, '\\\@<!\%o47', 1)) % 2
      let qinit = 2
    elseif qinit == 2 && len(split(line, '\\\@<!"', 1)) % 2
      let line = substitute(
            \ line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
      let qinit = 0
    elseif qinit == 1 && len(split(line, '\\\@<!\%o47', 1)) % 2
      let line = substitute(
            \ line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
      let qinit = 0
    endif
    let qinitdic[snum] = qinit
    let snum += 1
  endwhile
  let qinitdic['line'] = line

  return qinitdic
endfunction

function s:GetInitQuote(line)
  let init = 0
  let line = substitute(
            \ a:line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
  let dq = match(line, '\\\@<!"')
  let sq = match(line, '\\\@<!\%o47')
  if dq > -1 && sq < 0 || dq > -1 && dq < sq
    let init = 2
  elseif sq > -1 && dq < 0 || sq > -1 && sq < dq
    let init = 1
  endif

  return init
endfunction

function s:SkipQuoteLine(line, lnum, qinitdic)
  let lnum = a:lnum
  let pnum = lnum
  let init = a:qinitdic[lnum] % 2
  while (init && a:qinitdic[lnum] % 2 || !init && a:qinitdic[lnum])
        \ && s:GetPrevNonBlank(lnum)
    let pnum = lnum
    let lnum = s:prev_lnum
  endwhile
  unlet! s:prev_lnum
  let line = getline(pnum) . a:line

  return [line, pnum]
endfunction

function s:InsideComment(lnum)
  let line = getline(a:lnum)
  let cnum = len(line)
  let squote = 0
  let dquote = 0
  let bslash = 0
  let ssum = 0
  let sum = matchend(line, '\%(\${\h\w*#\=\|\${\=\|\\\)#')
  while sum > 0
    let ssum = sum
    let sum = matchend(line, '\%(\${\h\w*#\=\|\${\=\|\\\)#', sum)
  endwhile
  let sum = 0
  while sum < cnum
    let str = strpart(line, sum, 1)
    if str == '#' && !squote && !dquote && !bslash && sum > ssum
      return sum
    elseif str == '\'
      let bslash += 1
    elseif str == "'" && !squote && !dquote
      let squote = 1
      let bslash = 0
    elseif str == "'" && squote
      let squote = 0
      let bslash = 0
    elseif str == '"' && !squote && !dquote && !bslash
      let dquote = -1
    elseif str == '"' && !squote && !dquote && bslash
      let dquote = bslash
      let bslash = 0
    elseif str == '"' && (dquote < 0 && !bslash || dquote == bslash)
      let dquote = 0
      let bslash = 0
    else
      let bslash = 0
    endif
    let sum += 1
  endwhile

  return 0
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: set sts=2 sw=2 expandtab smarttab:
