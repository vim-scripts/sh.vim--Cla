" Vim indent file
" Language:         Shell Script
" Maintainer:       Clavelito <maromomo@hotmail.com>
" Id:               $Date: 2013-02-06 13:05:40+09 $
"                   $Revision: 1.21 $


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
  let [pline, pnum] = s:SkipCommentLine(line, lnum, 1)
  let ind = indent(lnum)
  let cind = indent(v:lnum)

  let ind = s:MorePrevLineIndent(line, pline, pnum, ind, cline, cind)
  let ind = s:PrevLineIndent(line, lnum, pline, ind, cline, cind)
  let ind = s:CurrentLineIndent(cline, ind, cind)
  let ind = s:InsideHereDocIndent(cline, ind, cind)

  return ind
endfunction

function s:MorePrevLineIndent(line, pline, pnum, ind, cline, cind)
  let ind = a:ind
  if a:pline !~ '\\$' && a:line =~ '\\$'
    let ind = ind + &sw
  elseif a:pline =~ '\\$' && a:line !~ '\\$'
    let ind = ind - &sw
  elseif a:pline =~# '^\s*IFS=\%(\%o47\|"\)'
        \ && a:pline !~# 'IFS=\(\%o47\|"\).*\1'
    let ind = s:EndedIfsLineIndent(a:pline, a:pnum, ind, a:cline, a:cind)
  endif

  return ind
endfunction

function s:PrevLineIndent(line, lnum, pline, ind, cline, cind)
  let ind = a:ind
  if a:line =~# '^\s*\%(if\|then\|else\|elif\)\>' && a:line !~# ';\s*\<fi\>'
        \ || (a:line =~# '^\s*\%(do\|while\|until\|for\)\>'
        \ || a:line =~# '\%(|\|;\)\s*\%(while\|until\)\>')
        \ && a:line !~# ';\s*\<done\>'
        \ || a:line =~ '{\s*\%(#.*\)\=$'
    let ind = ind + &sw
  elseif a:line =~# '^\s*case\>' && a:line !~# ';;\s*\<esac\>'
        \ && a:cline =~ '^\s*#'
    let ind = s:InsideCaseIndent(ind, 0)
  elseif a:line =~# '^\s*case\>' && a:line !~# ';;\s*\<esac\>'
        \ && a:cline !~ '^\s*#'
    let ind = s:InsideCaseIndent(ind, 1)
  elseif a:line =~ '^\s*[^(]\+\s*)\s*\%(#.*\)\=$'
        \ && (a:pline =~# '^\s*case\>' || a:pline =~ ';;\s*\%(#.*\)\=$')
    let ind = ind + &sw
  elseif a:line =~ ';;\s*\%(#.*\)\=$' && a:line !~ '^\s*[^(]\+\s*)'
    let ind = ind - &sw
  elseif a:line =~# '^\s*IFS=\%(\%o47\|"\)' && a:line !~# 'IFS=\(\%o47\|"\).*\1'
    let ind = a:cind
  elseif a:line =~# '^\s*IFS='
    let ind = s:EndedIfsLineIndent(a:line, a:lnum, ind, a:cline, a:cind)
  elseif a:line =~ '^\t*[A-Za-z0-9*-/!%:=?@\[\]^_{}~]\+$'
        \ && a:line
        \ !~# '^\t*\%(}\|fi\|done\|esac\|echo\|shift\|continue\|break\|exit\)$'
    let [sstr, estr] = s:GetHereDocPairLine1(a:line)
    let ind = s:ClosePairIndent(sstr, estr, a:lnum, ind, a:cline, a:cind, 1)
  endif

  return ind
endfunction

function s:CurrentLineIndent(cline, ind, cind)
  let ind = a:ind
  if a:cline =~# '^\s*case\>' && a:cline !~# ';;\s*\<esac\>'
    let lnum = nextnonblank(v:lnum + 1)
    if lnum > 0
      let s:case_labels_ind = indent(lnum) - a:cind
    endif
  elseif a:cline =~# '^\s*\%(then\|do\|else\|elif\|fi\|done\)\>'
        \ || a:cline =~ '^\s*}'
    let ind = ind - &sw
  elseif a:cline =~# '^\s*esac\>'
    let ind = s:ClosePairIndent('\C^\s*case\>', '\C^\s*esac\>', v:lnum, ind,
          \ a:cline, a:cind, 0)
  elseif a:cline =~ '^#' || a:cline =~# '^IFS='
        \ || a:cline =~ '<<[^-]' && a:cind == 0
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

function s:EndedIfsLineIndent(line, lnum, ind, cline, cind)
  let ind = a:ind
  let [line, lnum] = s:SkipCommentLine(a:line, a:lnum, 1)
  let [pline, pnum] = s:SkipCommentLine(line, lnum ,1)
  if lnum > 0
    let ind = indent(lnum)
    let ind = s:MorePrevLineIndent(line, pline, pnum, ind, a:cline, a:cind)
    let ind = s:PrevLineIndent(line, lnum, pline, ind, a:cline, a:cind)
  endif

  return ind
endfunction

function s:ClosePairIndent(sstr, estr, lnum, ind, cline, cind, prev)
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
      let ind = indent(lnum)
      let ind = s:MorePrevLineIndent(line, pline, pnum, ind, a:cline, a:cind)
      let ind = s:PrevLineIndent(line, lnum, pline, ind, a:cline, a:cind)
    endif
  elseif lnum > 0
    let ind = indent(lnum)
  endif

  return ind
endfunction

function s:SkipCommentLine(line, lnum, prev)
  let line = a:line
  let lnum = a:lnum
  while line =~ '^\s*#'
    let lnum = prevnonblank(lnum - 1)
    let line = getline(lnum)
  endwhile
  if a:prev
    let lnum = prevnonblank(lnum - 1)
    let line = getline(lnum)
    while line =~ '^\s*#'
      let lnum = prevnonblank(lnum - 1)
      let line = getline(lnum)
    endwhile
  endif

  return [line, lnum]
endfunction

function s:InsideCaseIndent(ind, unlet)
  let ind = a:ind
  if exists("g:sh_indent_case_labels") && g:sh_indent_case_labels
    let ind = ind + &sw
  elseif exists("s:case_labels_ind") && s:case_labels_ind
    let ind = ind + s:case_labels_ind
  endif
  if exists("s:case_labels_ind") && a:unlet
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

" vim: set sts=2 sw=2 expandtab:
