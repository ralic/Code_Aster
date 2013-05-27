subroutine jeimpa(unit, nomlu, com)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjlide.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/u2mesk.h'
    integer :: unit
    character(len=*) :: nomlu, com
!
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
!
    integer :: numec
    common /inumje/  numec
    character(len=24) :: nomec
    common /knomje/  nomec
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibacol, ic, icol, id, ilon, ipgcex
    integer :: ixiadd, ixlong, j, jcol, jdocu, jgenr, jlon
    integer :: jorig, jrnom, jtype, k, n, nnac, nnaci
    integer :: nnao
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    character(len=8) :: nume, nome
!
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     -----------------------------------------------------------------
    integer :: iddeso, idiadd, idlong
    parameter    (  iddeso = 1 ,idiadd = 2  ,&
     &                            idlong = 7  )
!
    character(len=72) :: coml
    character(len=32) :: noml32
    character(len=33) :: cval
    integer :: icre, iret, ival
!
    parameter     ( nnao = 15 )
    parameter     ( nnac = 6  )
    character(len=8) :: nac(nnac), nao(nnao)
    character(len=1) :: tac(nnac), tao(nnao), genri
    integer :: lac(nnac), lao(nnao)
    logical :: tab1(5, 4), tab2(5, 4), tab3(2, 3)
    logical :: lconst, lconti, lcol
!
    data             nume       , nome&
     &               / '$$XNUM  ' , '$$XNOM  '  /
    data nao     /&
     &     'CLAS    ' , 'GENR    ' , 'TYPE    ' , 'LTYP    ' ,&
     &     'DOCU    ' , 'DATE    ' ,              'LONMAX  ' ,&
     &     'NOMMAX  ' , 'LONUTI  ' , 'NOMUTI  ' , 'LONO    ' ,&
     &     'IADM    ' , 'IADD    ' , 'LADD    ' , 'USAGE   ' /
    data nac     /   'ACCES   ' , 'STOCKAGE' , 'MODELONG' ,&
     &                 'NMAXOC  ' , 'NUTIOC  ' , 'LONT    ' /
    data tao     /&
     &     'K'      , 'K'      , 'K'      , 'I'      ,&
     &     'K'      , 'I'      , 'I'      ,&
     &     'I'      , 'I'      , 'I'      , 'I'      ,&
     &     'I'      , 'I'      , 'I'      , 'K'      /
    data tac     /&
     &  'K'    , 'K'     , 'K'     , 'I'     ,'I' ,    'I' /
    data lao     /&
     &     1        , 1        , 1        , 0        ,&
     &     4        , 0        , 0        ,&
     &     0        , 0        , 0        , 0        ,&
     &     0        , 0        , 0        , 3        /
    data lac     /&
     &    33        ,   8        ,   33       , 0          , 0  , 0   /
! 1 : CONT CSTE - 2 : DISP CSTE - 3 : CONT VARI - 4 : DISP VARI
!     - IRET = 3 - CONDITION D'ACCES A LONO / IADM / IADD / LADD / USAGE
    data (( tab1(i,j),i=1,5),j=1,4)    /&
     &     .false.  , .false.  , .false.  , .false.  , .true.  ,&
     &     .true.   , .true.   , .true.   , .true.   , .true.  ,&
     &     .false.  , .false.  , .false.  , .false.  , .true.  ,&
     &     .true.   , .true.   , .true.   , .true.   , .true.  /
!     - IRET = 2 - CONDITION D'ACCES A LONO / IADM / IADD / LADD / USAGE
    data (( tab2(i,j),i=1,5),j=1,4)    /&
     &     .true.  , .true.   , .true.   , .true.   , .true.  ,&
     &     .false. , .false.  , .false.  , .false.  , .false. ,&
     &     .true.  , .true.   , .true.   , .true.   , .true.  ,&
     &     .false. , .false.  , .false.  , .false.  , .false. /
!     ------------------- CONDITION D'ACCES A LON... / NOM...
    data (( tab3(i,j),i=1,2),j=1,3)    /&
     &     .true.  , .false. ,&
     &     .true.  , .false. ,&
     &     .false. , .true.  /
! DEB -----------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -2
    noml32 = nomlu
    coml = com
    icre = 0
    iret = 0
    jcol = 1
    ilon = 1
    lconst=.false.
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
    else if (iret .eq. 1) then
        lcol = .false.
        ic = iclaos
        id = idatos
    else if (iret .eq. 2) then
        ic = iclaco
        lcol = .true.
        call jjallc(iclaco, idatco, 'L', ibacol)
        id = iszon(jiszon + ibacol + iddeso )
        ixlong = iszon(jiszon + ibacol + idlong )
        lconst = ixlong .eq. 0
        ixiadd = iszon(jiszon + ibacol + idiadd )
        lconti = ixiadd .eq. 0
        if (.not. lconti .and. lconst) jcol = 2
        if (lconti .and. .not. lconst) jcol = 3
        if (.not. lconti .and. .not. lconst) jcol = 4
        if (noml32(25:32) .ne. '        ') then
            call jjcroc(noml32(25:32), icre)
            iret = 3
        endif
    endif
    genri = genr ( jgenr(ic) + id )
    jlon = 1
    if (genri .eq. 'V') jlon = 2
    if (genri .eq. 'N') jlon = 3
!
    write(unit,'(A)') 'JEIMPA  IMPRESSION DES ATTRIBUTS DE >'&
     &                  //noml32(1:24)//'<'
    write(unit,'(A1,A72)')  ' ',coml
    if (iret .eq. 3) then
        if (noml32(25:32) .eq. nome) then
            write(unit,'(A,A8)') 'NOM OC',nomec
        else if (noml32(25:32) .eq. nume) then
            write(unit,'(A,I12)') 'NUM OC',numec
        endif
    endif
    if (iret .eq. 2) then
        nnaci = nnac
        if (.not. lconti) nnaci = nnac - 1
        do 10 k = 1, nnaci
            call jelira(noml32, nac(k), ival, cval)
            if (tac(k) .eq. 'I') then
                write(unit,'(A8,I12)') nac(k),ival
            else
                write(unit,'(A8,A)') nac(k),cval(1:lac(k))
            endif
10      continue
    endif
    do 20 k = 1, nnao
        icol = k - 10
        if (nao(k)(1:3) .eq. 'LON') ilon = 1
        if (nao(k)(1:3) .eq. 'NOM') ilon = 2
        if ((k.le.6) .or. (k.gt.6 .and. k.le.10 .and. lconst .and. tab3(ilon,jlon) ) .or.&
            ((iret.eq.1.or.iret.eq.3) .and. ( k.gt.6 .and. k.le.10) .and. tab3(ilon,jlon))&
            .or. ( iret.eq.2 .and. (k.gt.10.and.tab2(icol,jcol))) .or.&
            ( iret.eq.3 .and. (k.gt.10.and.tab1(icol,jcol))) .or.&
            ( iret.eq.1 .and. (k.gt.10) )) then
            call jelira(noml32, nao(k), ival, cval)
            if (tao(k) .eq. 'I') then
                write(unit,'(A8,I12)') nao(k),ival
            else
                write(unit,'(A8,A)') nao(k),cval(1:lao(k))
            endif
        endif
20  end do
    if (lcol) then
        call jjlide('JEIMPA', noml32(1:24), 2)
    endif
    ipgc = ipgcex
end subroutine
