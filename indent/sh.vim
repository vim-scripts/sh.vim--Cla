" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2013-06-14 01:46:38+09 $
"                   $Revision: 1.32 $
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
  if line =~# '\\$\|\h\w*=\%o47$'&& cline =~ '^\s*$'
        \ && !s:InsideSingleQuote(lnum) && s:InsideSingleQuote(v:lnum)
    return 0
  elseif s:InsideSingleQuote(lnum) && !s:InsideSingleQuote(v:lnum)
    let [line, lnum] = s:SkipSingleQuoteLine(line, lnum)
  elseif !&autoindent && cline =~ '^$' && s:InsideSingleQuote(v:lnum)
    return indent(lnum)
  elseif s:InsideSingleQuote(v:lnum)
    return indent(v:lnum)
  endif

  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  if s:InsideSingleQuote(pnum) && !s:InsideSingleQuote(lnum)
    let [pline, pnum] = s:SkipSingleQuoteLine(pline, pnum)
  endif

  let ind = indent(lnum)
  let cind = indent(v:lnum)

  let ind = s:MorePrevLineIndent(pline, line, ind)
  let ind = s:PrevLineIndent(line, lnum, pline, cline, ind)
  let ind = s:CurrentLineIndent(cline, ind, cind)
  let ind = s:InsideHereDocIndent(cline, ind, cind)

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
  let line = a:line
  if line =~ '\n'
    if line =~# '\%(\n.*\)\@<=|\s*\%(while\|until\)\>'
          \ && line !~# ';\s*\<done\>'
      let ind = ind + &sw
    endif
    let line = substitute(line, '\n.*$', '', '')
  endif
  if line =~# '^\s*\%(if\|then\|else\|elif\)\>' && line !~# ';\s*\<fi\>'
        \ || (line =~# '^\s*\%(do\|while\|until\|for\)\>'
        \ || line =~# '\%(|\|;\)\s*\%(while\|until\)\>'
        \ || line =~# 'for\>\s\+\h\w*\s\+in\>') && line !~# ';\s*\<done\>'
        \ || line =~ '^\s*{\s*\%(#.*\)\=$'
        \ || line =~ '^\h\w*\s*(\s*)\s*{\s*\%(#.*\)\=$'
        \ || line =~ '\%(&&\|||\)\s*{\s*\%(#.*\)\=$'
        \ || line =~ '^\s*[^(]\+\s*)\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let ind = ind + &sw
  elseif line =~# '^\s*case\>' && line !~# ';;\s*\<esac\>'
    let ind = s:InsideCaseIndent(ind, a:cline)
  elseif line =~ ';;\s*\%(#.*\)\=$' && line !~ '^\s*[^(]\+\s*)'
    let ind = ind - &sw
  elseif line =~ '^\t*[A-Za-z0-9*-/!%:=?@\[\]^_{}~]\+$'
        \ && line !~# '\%(}\|fi\|done\|esac\|echo\|shift\|continue' .
        \ '\|break\|exit\|return\)$'
    let [sstr, estr] = s:GetHereDocPairLine1(line)
    let ind = s:ClosePairIndent(sstr, estr, a:lnum, a:cline, ind, 1)
  endif

  return ind
endfunction

function s:CurrentLineIndent(cline, ind, cind)
  let ind = a:ind
  if a:cline =~# '^\s*case\>' && a:cline !~# ';;\s*\<esac\>'
        \ && exists("g:sh_indent_case_labels") && !g:sh_indent_case_labels
    let lnum = nextnonblank(v:lnum + 1)
    if lnum > 0
      let s:case_labels_ind = indent(lnum) - a:cind
    endif
  elseif a:cline =~# '^\s*\%(then\|do\|else\|elif\|fi\|done\)\>'
        \ || a:cline =~ '^\s*}'
    let ind = ind - &sw
  elseif a:cline =~# '^\s*esac\>'
        \ && exists("g:sh_indent_case_labels") && g:sh_indent_case_labels
    let ind = ind - &sw
  elseif a:cline =~# '^\s*esac\>'
    let ind = s:ClosePairIndent(
          \ '\C^\s*case\>', '\C^\s*esac\>', v:lnum, a:cline, ind, 0)
  elseif a:cline =~ '^#'
        \ || a:cind == 0 && a:cline =~ '<<[^-]'
        \ || a:cind == 0 && len(split(a:cline, "'", 1)) % 2 == 0
    let ind = 0
  endif

  return ind
endfunction

function s:InsideHereDocIndent(cline, ind, cind)
  let ind = a:ind
  let lnum = 0
  let onum = 0
  let save_cursor = getpos(".")
  while search('<<-\=\s*\%("\|\%o47\|\\\)\=[A-Za-z0-9*-/!%:=?@\[\]^_{}~]\+' .
        \ '\%("\|\%o47\)\=', 'bW')
    let snum = line(".")
    let [sstr, estr] = s:GetHereDocPairLine2()
    let lnum = searchpair(sstr, '', estr, 'nW')
    if lnum >= get(save_cursor, 1) || lnum < 1 || lnum < onum
      break
    endif
    let onum = lnum
  endwhile
  call setpos('.', save_cursor)
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
  endif

  return ind
endfunction

function s:ClosePairIndent(sstr, estr, lnum, cline, ind, prev)
  let ind = a:ind
  let save_cursor = getpos(".")
  call cursor(a:lnum, 1)
  let lnum = searchpair(a:sstr, '', a:estr, 'bW')
  call setpos('.', save_cursor)
  if lnum > 0 && a:prev
    let ind = indent(lnum)
    let line = getline(lnum)
    let [line, lnum] = s:SkipCommentLine(line, lnum, 1)
    let [pline, pnum] = s:SkipCommentLine(line, lnum ,1)
    if lnum > 0
      if s:InsideSingleQuote(lnum) && !s:InsideSingleQuote(v:lnum)
        let [line, lnum] = s:SkipSingleQuoteLine(line, lnum)
        let [pline, pnum] = s:SkipCommentLine(line, lnum ,1)
      endif
      let ind = indent(lnum)
      let ind = s:MorePrevLineIndent(pline, line, ind)
      let ind = s:PrevLineIndent(line, lnum, pline, a:cline, ind)
    endif
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

  if s:prev_lnum
    return 1
  else
    return 0
  endif
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
  let estr = matchstr(a:line, '[A-Za-z0-9*-/!%:=?@\[\]^_{}~]\+$')
  let sstr = '\C<<-\=\s*\%("\|\%o47\|\\\)\=\M' . estr . '\m\%("\|\%o47\)\='
  if a:line =~ '^\t\+'
    let estr = '\C^\t*\M' . estr . '\m$'
  else
    let estr = '\C^\M' . estr . '\m$'
  endif

  return [sstr, estr]
endfunction

function s:GetHereDocPairLine2()
  let line = getline(".")
  let estr = substitute(line, '^.*<<-\=\s*\%("\|\%o47\|\\\)\=', '', '')
  let estr = substitute(estr,
        \ '^\([A-Za-z0-9*-/!%:=?@\[\]^_{}~]\+\)\%("\|\%o47\)\=.*$', '\1', '')
  if line =~ '<<-'
    let sstr = '\C<<-\=\s*\%("\|\%o47\|\\\)\=\M' . estr . '\m\%("\|\%o47\)\='
    let estr = '\C^\t*\M' . estr . '\m$'
  else
    let sstr = '\C<<\s*\%("\|\%o47\|\\\)\=\M' . estr . '\m\%("\|\%o47\)\='
    let estr = '\C^\M' . estr . '\m$'
  endif

  return [sstr, estr]
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

function s:InsideSingleQuote(lnum)
  let line = ""
  let snum = 0
  let mnum = search("'", 'nbW')
  let slist = [search('^\h\w*\s*(\s*)\s*{\=\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*fi\>\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*esac\>\s*\%(#.*\)\=$', 'nbW'),
        \ search('^\s*done\>\s*\%(#.*\)\=$', 'nbW')]
  for enum in slist
    if enum > snum
      let snum = enum
    endif
  endfor
  if mnum <= snum
    return 0
  endif

  let snum += 1
  while snum < a:lnum
    let nline = getline(snum)
    let snum += 1
    if nline !~ "'"
      continue
    elseif nline =~ '^\s*#' && len(split(line, "'", 1)) % 2
      continue
    elseif nline =~ '#' && len(split(line, "'", 1)) % 2
      let nline = substitute(nline, "'[^']\\+'" . '\|"[^"]\+"', '', 'g')
      let nline = substitute(nline, '^\(.*\)#.*$', '\1', '')
    elseif nline =~ '"' && len(split(line, "'", 1)) % 2
      let nline = substitute(nline, '"[^"]\+"', '', 'g')
    endif
    let line = line . nline
  endwhile

  if len(split(line, "'", 1)) % 2
    return 0
  else
    return 1
  endif
endfunction

function s:SkipSingleQuoteLine(line, lnum)
  let lnum = a:lnum
  while s:InsideSingleQuote(lnum)
    let lnum = prevnonblank(lnum - 1)
  endwhile
  let line = getline(lnum)
  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  if pnum && s:InsideSingleQuote(pnum)
    let [line, lnum] = s:SkipSingleQuoteLine(pline, pnum)
  endif
  if lnum && indent(lnum) == 0
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
