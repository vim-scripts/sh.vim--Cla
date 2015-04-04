" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2015-04-04 19:25:54+09 $
"                   $Revision: 3.0 $


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

function GetShIndent()
  let lnum = prevnonblank(v:lnum - 1)
  if lnum == 0
    return 0
  endif

  let line = getline(lnum)
  let cline = getline(v:lnum)

  let [line, lnum] = s:SkipCommentLine(line, lnum, 0)
  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  let ind = indent(lnum)
  let ind = s:MorePrevLineIndent(pline, line, ind)
  let ind = s:InsideCaseLabelIndent(pline, line, ind)
  let ind = s:PrevLineIndent(line, lnum, pline, ind)
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

function s:PrevLineIndent(line, lnum, pline, ind)
  let ind = a:ind
  if a:line =~ '^\s*[{(]\s*\%(#.*\)\=$'
        \ || a:line =~ '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=$'
        \ || a:line =~ '\%(;\|&&\|||\)\s*\%({\|(\)\s*\%(#.*\)\=$'
    let ind = ind + &sw
  elseif a:line =~ '|\|&\|`\|('
        \ || a:line =~ '^\s*[^(].\{-})' && a:line !~ ';;\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let line = s:HideAnyItemLine(a:line, a:pline)
    for line in split(line, '|\|&\|`\|(')
      let ind = s:PrevLineIndent2(line, ind)
    endfor
  else
    let line = s:HideAnyItemLine(a:line, a:pline)
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

function s:HideAnyItemLine(line, pline)
  let line = substitute(
        \ a:line, "'[^']*'" . '\|\(\\\@<!\\*"\).\{-}\\\@<!\1', '', 'g')
  let line = substitute(line, '\\\@<!#.*$', '', '')
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

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: set sts=2 sw=2 expandtab smarttab:
