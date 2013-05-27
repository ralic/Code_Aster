subroutine jeecra(nomlu, catr, ival, cval)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    include 'asterfort/jjallc.h'
    include 'asterfort/jjalls.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjecrs.h'
    include 'asterfort/jjprem.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomlu, catr, cval
    integer :: ival
! ----------------------------------------------------------------------
! ROUTINE UTILISATEUR D'AFFECTATION D'UN ATTRIBUT
!
! IN  NOMLU  : NOM DE L'OBJET JEVEUX
! IN  CATR   : NOM DE L'ATTRIBUT
! IN  IVAL   : VALEUR EN ENTIER DE L'ATTRIBUT
! IN  CVAL   : VALEUR EN CARACTERE DE L'ATTRIBUT
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmi, iadyn, iblong, iblono, ibluti, ic, id
    integer :: il0, il1, ixlono, ixluti, jcara, jdate, jdocu
    integer :: jgenr, jhcod, jiadd, jiadm, jitab, jlong
    integer :: jlono, jltyp, jluti, jmarq, jorig, jrnom, jtype
    integer :: longi, longj, lonoi, lonoj, lonok, lont, lonti
    integer :: ltypi, n, nbl, nhc, nmaxi
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    character(len=1) :: genri, typei
    character(len=8) :: catrlu
    logical :: lconst, lconti, llong, lluti
    integer :: icre, iret, itab(1), jtab, irt
    integer :: ibacol, ixiadd, ixdeso, ixlong
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idlong, idlono, idluti
    parameter    ( ivnmax = 0 , iddeso = 1 ,idiadd = 2 ,&
     &               idlong = 7 ,&
     &               idlono = 8 , idluti = 9 )
    integer :: ilorep, ideno, ilnom, ilmax, iluti, idehc
    parameter      ( ilorep=1,ideno=2,ilnom=3,ilmax=4,iluti=5,idehc=6)
! DEB ------------------------------------------------------------------
    real(kind=8) :: r8bid
!
    catrlu = catr
    noml32 = nomlu
    irt = 0
!
! --- CAS GENERAL
!
    icre = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
    else if (iret .eq. 1) then
        ic = iclaos
        id = idatos
        lconst = .true.
        lconti = .false.
        ixlong = id
        ixlono = id
        ixluti = id
    else
        ic = iclaco
        id = idatco
        call jjallc(ic, id, 'E', ibacol)
        if (noml32(25:32) .ne. '        ') then
            iret = 3
            call jjcroc(noml32(25:32), icre)
        endif
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        id = ixdeso
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        lconti = ( ixiadd .eq. 0 )
        ixlong = iszon ( jiszon + ibacol + idlong )
        ixlono = iszon ( jiszon + ibacol + idlono )
        ixluti = iszon ( jiszon + ibacol + idluti )
        lconst = (ixlong .eq. 0 )
        nmaxi = iszon (jiszon + ibacol + ivnmax )
    endif
!
    genri = genr ( jgenr(ic) + id )
    typei = type ( jtype(ic) + id )
    if (catrlu .eq. 'LONT    ') then
        if (.not. lconti) then
            call u2mesk('F', 'JEVEUX_98', 1, catrlu)
        else
            llong = .false.
            lluti = .false.
        endif
    else
        llong = ( catrlu(4:6) .eq. 'MAX' )
        lluti = ( catrlu(4:6) .eq. 'UTI' )
        if ((genri .ne. 'N' .and. catrlu(1:3).eq. 'NOM') .or.&
            (genri .eq. 'N' .and. catrlu(1:4).eq. 'NOMU') .or.&
            (genri .ne. 'V' .and. catrlu(1:4).eq. 'LONM') .or.&
            (genri .ne. 'V' .and. catrlu(1:4).eq. 'LONU')) then
            call u2mesk('F', 'JEVEUX_99', 1, genri)
        endif
    endif
!
    if (catrlu .eq. 'LONT    ' .and. lconti) then
        lono( jlono(ic) + id ) = ival
        if (lconst) long( jlong(ic) + id ) = ival / nmaxi
    else if (catrlu .eq. 'DATE    ') then
        date ( jdate(ic) + id ) = ival
    else if (catrlu .eq. 'DOCU    ') then
        docu ( jdocu(ic) + id ) = cval
    else if (lconst) then
        if (llong) then
            lonoi = lono ( jlono(ic) + id )
            if (catrlu .eq. 'LONMAX  ' .or. catrlu .eq. 'NOMMAX  ') then
                longi = long ( jlong(ic) + id )
                longj = ival
                if (ival .le. 0) then
                    call u2mesg('F', 'JEVEUX1_67', 1, catrlu, 1,&
                                ival, 0, r8bid)
                endif
            endif
            if (longi .ne. 0) then
                call u2mesk('F', 'JEVEUX1_01', 1, catrlu)
            else
                long ( jlong(ic) + id ) = longj
                if (lonoi .ne. 0 .and. iret .eq. 1) then
                    call u2mesk('F', 'JEVEUX1_02', 1, catrlu)
                else
                    if (genri .eq. 'V') then
                        lono ( jlono(ic) + id ) = longj
                    else if (genri .eq. 'N') then
                        ltypi = ltyp ( jltyp(ic) + id )
                        lonok = (idehc + jjprem(longj,irt))*lois+( longj+1 )*ltypi
                        if (mod(lonok,ltypi) .gt. 0) then
                            lonok = (lonok/ltypi + 1 )
                        else
                            lonok = lonok/ltypi
                        endif
                        lono ( jlono(ic) + id ) = lonok
                        luti ( jluti(ic) + id ) = 0
                        if (iadm(jiadm(ic)+2*id-1) .eq. 0) then
                            nbl = lonok*ltypi
                            call jjalls(nbl, ic, genri, typei, ltypi,&
                                        'INIT', itab, jtab, iadmi, iadyn)
                            iadm(jiadm(ic)+2*id-1) = iadmi
                            iadm(jiadm(ic)+2*id ) = iadyn
                            call jjecrs(iadmi, ic, id, 0, 'E',&
                                        imarq( jmarq(ic)+2*id-1))
                            nhc = jjprem(ival,irt)
                            jitab = jiszon + iadmi - 1
                            iszon(jitab + ilorep ) = nhc
                            iszon(jitab + ideno ) = (idehc+nhc)*lois
                            iszon(jitab + ilnom ) = ltypi
                            iszon(jitab + ilmax ) = ival
                            iszon(jitab + iluti ) = 0
                            iszon(jitab + idehc ) = idehc
                        endif
                    endif
                    if (lconti) then
                        if (lonoi .ne. 0 .and. lonoi .lt. nmaxi*lono(jlono( ic)+id)) then
                            call u2mesk('F', 'JEVEUX1_03', 1, catrlu)
                        else
                            lono (jlono(ic)+id) = nmaxi * lono (&
                            jlono(ic) + id )
                        endif
                    endif
                endif
            endif
        else if (lluti) then
            if (catrlu .eq. 'LONUTI  ') then
                luti ( jluti(ic) + id ) = ival
            endif
        else
            call u2mesk('F', 'JEVEUX1_04', 1, catrlu)
        endif
    else if (iret .eq. 3) then
        if (llong .and. .not. lconst) then
            iblong = iadm ( jiadm(ic) + 2*ixlong-1 )
            iblono = iadm ( jiadm(ic) + 2*ixlono-1 )
            if (lconti) then
                if (idatoc .eq. 1) then
                    if (iszon (jiszon+iblono-1+idatoc) .eq. 0) then
                        iszon (jiszon+iblono-1+idatoc) = 1
                    endif
                endif
                il1 = jiszon + iblono - 1 + idatoc + 1
                il0 = jiszon + iblono - 1 + idatoc
                if (iszon(il0) .eq. 0) then
                    call u2mesk('F', 'JEVEUX1_05', 1, catrlu)
                else
                    lonti = iszon(il0)
                    lonoi = 0
                    if (iszon(il1) .ne. 0) then
                        lonti = max ( iszon(il1) , iszon(il0) )
                        lonoi = max ( iszon ( il1 ) - iszon(il0) , 0 )
                    endif
                endif
            else
                lonoi = iszon ( jiszon + iblono - 1 + idatoc )
            endif
            if (lonoi .ne. 0) then
                call u2mesk('F', 'JEVEUX1_06', 1, catrlu)
            endif
            if (catrlu .eq. 'LONMAX  ') then
                longi = iszon ( jiszon + iblong - 1 + idatoc )
                longj = ival
                lonoj = longj
            endif
            if (longi .ne. 0) then
                call u2mesk('F', 'JEVEUX1_01', 1, catrlu)
            else
                if (lconti) then
                    lont=lono( jlono(ic) + id )
                    if (lont .ne. 0 .and. lonti -1 + lonoj .gt. lont) then
                        call u2mesk('F', 'JEVEUX1_07', 1, catrlu)
                    else
                        iszon(jiszon+iblono-1+idatoc+1) = lonti + lonoj
                    endif
                else
                    iszon(jiszon+iblono-1+idatoc) = lonoj
                endif
                iszon(jiszon+iblong-1+idatoc) = longj
                luti ( jluti(ic)+ixlono ) = 1 + luti( jluti(ic)+&
                ixlono )
            endif
        else if (lluti) then
            ibluti = iadm ( jiadm(ic) + 2*ixluti-1 )
            if (catrlu .eq. 'LONUTI  ') then
                iszon ( jiszon + ibluti - 1 + idatoc ) = ival
            endif
        endif
    else
        call u2mesk('F', 'JEVEUX1_04', 1, catrlu)
    endif
! FIN ------------------------------------------------------------------
end subroutine
