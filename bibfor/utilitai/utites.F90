subroutine utites(label1, label2, type, nbref, refi,&
                  refr, refc, vali, valr, valc,&
                  epsi, crit, ific, llab, ssigne)
    implicit       none
    include 'asterfort/assert.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/utcovt.h'
    integer :: vali, nbref, refi(nbref), ific
    character(len=*) :: label1, label2, type, crit, ssigne
    real(kind=8) :: valr, refr(nbref), epsi
    complex(kind=8) :: valc, refc(nbref)
    logical :: llab
! ----------------------------------------------------------------------
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
!  IN  : K1  : TYPE   : TYPE DE VALEUR A TESTER 'R', OU 'C'
!  IN  : I   : NBREF  : NOMBRE DE VALEURS DE REFERENCE (=1 SOUVENT)
!  IN  : R8  : REFR   : VALEUR(S) REELLE(S) DE REFERENCE
!  IN  : C16 : REFC   : VALEUR(S) COMPLEXE(S) DE REFERENCE
!  IN  : R8  : VALR   : VALEUR REELLE A TESTER ( ASTER )
!  IN  : C16 : VALC   : VALEUR COMPLEXE A TESTER ( ASTER )
!  IN  : K8  : CRIT   : PRECISION 'RELATIVE' OU 'ABSOLUE'
!  IN  : R8  : EPSI   : PRECISION ESPEREE
!  IN  : I   : IFIC   : NUMERO LOGIQUE DU FICHIER DE SORTIE
!  IN  : L   : LLAB   : FLAG IMPRESSION LABELS
!  OUT :                IMPRESSION SUR LISTING
! ----------------------------------------------------------------------
    integer :: i, imin, minvi, tmpi, nl, tvali(2), nl1, nl2
    parameter   (nl=6)
    character(len=4) :: testok, rela
    character(len=16) :: k120, k170, tcherr(2)
    character(len=24) :: tchval(2), tchvar(2), tchvac(2)
    character(len=48) :: lign2(nl), tchva2(2)
    real(kind=8) :: err, zero, cent, tmpr, minvr, minvc, tmpc
    real(kind=8) :: tvalr(2), terrr(2)
    complex(kind=8) :: zeroc, vtc
    logical :: lok
    data lign2/'REFERENCE','LEGENDE','VALE_REFE','VALE_CALC','ERREUR',&
     &           'TOLE'/
!     ------------------------------------------------------------------
!
    k120 = label1
    k170 = label2
    rela = crit
    cent = 100.0d0
    zero = 0.d0
    zeroc = dcmplx(0.d0,0.d0)
    testok = 'NOOK'
    tchvar(1) = ' '
    tchvar(2) = ' '
    tchvac(1) = ' '
    tchvac(2) = ' '
    tchva2(1) = ' '
    tchva2(2) = ' '
!
!-----------------
! --- CAS REEL ---
! ----------------
!
    if (type(1:1) .eq. 'R') then
!
        if (ssigne .eq. 'OUI') valr = abs(valr)
        minvr=abs(valr-refr(1))
        imin=1
        do 10 i = 1, nbref-1
            tmpr=abs(valr-refr(i+1))
            if (tmpr .lt. minvr) then
                tmpr=minvr
                imin=i+1
            endif
10      continue
!
        tvalr(1)=refr(imin)
        tvalr(2)=valr
        tvali(1)=0
        tvali(2)=0
        tchval(1)=' '
        tchval(2)=' '
        tcherr(1)=' '
        tcherr(2)=' '
!
        if (rela .eq. 'RELA') then
            lok = ( abs(valr-refr(imin)) .le. epsi * abs(refr(imin)) )
            if (refr(imin) .ne. zero) then
                err = (valr - refr(imin)) / refr(imin) *cent
            else
                if (lok) then
                    err = 0.d0
                else
                    err = 999.999999D0
                endif
            endif
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi*cent
        else
            lok = ( abs(valr - refr(imin)) .le. epsi )
            err = valr - refr(imin)
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi
        endif
!
        call utcovt('R', tvalr, tvali, terrr, rela,&
                    tchval, tcherr)
!
        nl1=lxlgut(tchval(1))
        nl2=lxlgut(tchval(2))
!
        if (nl1 .lt. 17) then
            if (nl2 .lt. 17) then
                if (llab) write(ific,1616)(lign2(i), i=1,nl)
                write(ific,5616)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
!
            else
                if (llab) write(ific,1624)(lign2(i), i=1,nl)
                write(ific,5624)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
            endif
        else
            if (nl2 .lt. 17) then
                if (llab) write(ific,2416)(lign2(i), i=1,nl)
                write(ific,6416)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
!
            else
                if (llab) write(ific,2424)(lign2(i), i=1,nl)
                write(ific,6424)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
            endif
        endif
!
!-------------------
! --- CAS ENTIER ---
! ------------------
!
    else if (type(1:1) .eq. 'I') then
!
        minvi=abs(vali-refi(1))
        imin=1
        do 20 i = 1, nbref-1
            tmpi=abs(vali-refi(i+1))
            if (tmpi .lt. minvi) then
                tmpi=minvi
                imin=i+1
            endif
20      continue
        if (ssigne .eq. 'OUI') vali = abs(vali)
        err = dble( vali - refi(imin) )
!
        tvalr(1)=0.d0
        tvalr(2)=0.d0
        tvali(1)=refi(imin)
        tvali(2)=vali
        tchval(1)=' '
        tchval(2)=' '
        tcherr(1)=' '
        tcherr(2)=' '
!
        if (rela .eq. 'RELA') then
            lok = ( abs( err ) .le. epsi*abs(refi(imin)) )
            if (refi(imin) .ne. 0) then
                err = ( vali - refi(imin) )*cent / refi(imin)
            else
                if (lok) then
                    err = 0.d0
                else
                    err = 999.999999D0
                endif
            endif
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi*cent
        else
            lok = ( abs( err ) .le. epsi )
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi
        endif
!
        call utcovt('I', tvalr, tvali, terrr, rela,&
                    tchval, tcherr)
!
        nl1=lxlgut(tchval(1))
        nl2=lxlgut(tchval(2))
!
        if (nl1 .lt. 17) then
            if (nl2 .lt. 17) then
                if (llab) write(ific,1616)(lign2(i), i=1,nl)
                write(ific,5616)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
!
            else
                if (llab) write(ific,1624)(lign2(i), i=1,nl)
                write(ific,5624)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
            endif
        else
            if (nl2 .lt. 17) then
                if (llab) write(ific,2416)(lign2(i), i=1,nl)
                write(ific,6416)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
!
            else
                if (llab) write(ific,2424)(lign2(i), i=1,nl)
                write(ific,6424)testok,k120,k170,tchval(1),tchval(2),&
                tcherr(1),tcherr(2)
            endif
        endif
!
!---------------------
! --- CAS COMPLEXE ---
! --------------------
!
    else if (type(1:1) .eq. 'C') then
!
        vtc = refc(1)
        if (ssigne .eq. 'OUI') then
            valc = abs(valc)
            vtc = abs(vtc)
        endif
        minvc=abs(valc-vtc)
        imin=1
!    --- NBREF > 1 N'EST PAS GERE PAR LE SUPERVISEUR...
        do 30 i = 1, nbref-1
            vtc = refc(i+1)
            if (ssigne .eq. 'OUI') vtc = abs(vtc)
            tmpc=abs(valc-vtc)
            if (tmpc .lt. minvc) then
                tmpc=minvc
                imin=i+1
            endif
30      continue
        vtc = refc(imin)
        if (ssigne .eq. 'OUI') vtc = abs(vtc)
!
        tvali(1)=0
        tvali(2)=0
        tchval(1)=' '
        tchval(2)=' '
        tcherr(1)=' '
        tcherr(2)=' '
!
        if (rela .eq. 'RELA') then
            lok = ( abs(valc-vtc) .le. epsi * abs(vtc))
            if (vtc .ne. zeroc) then
                err = abs(valc - vtc) / abs(vtc) *cent
            else
                if (lok) then
                    err = 0.d0
                else
                    err = 999.999999D0
                endif
            endif
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi*cent
        else
            lok = ( abs(valc - vtc) .le. epsi )
            err = abs(valc - vtc)
            if (lok) testok = ' OK '
            terrr(1)=err
            terrr(2)=epsi
        endif
!
        tvalr(1)=dble(vtc)
        tvalr(2)=dble(valc)
        call utcovt('R', tvalr, tvali, terrr, rela,&
                    tchvar, tcherr)
!
        tvalr(1)=dimag(vtc)
        tvalr(2)=dimag(valc)
        call utcovt('R', tvalr, tvali, terrr, rela,&
                    tchvac, tcherr)
!
        nl1=lxlgut(tchvar(1))
        nl2=lxlgut(tchvac(1))
        if (tchvac(1)(1:1) .eq. '-') then
            tchva2(1)=tchvar(1)(1:nl1)//tchvac(1)(1:nl2)//'j'
        else
            tchva2(1)=tchvar(1)(1:nl1)//'+'//tchvac(1)(1:nl2)//'j'
        endif
        nl1=lxlgut(tchvar(2))
        nl2=lxlgut(tchvac(2))
        if (tchvac(2)(1:1) .eq. '-') then
            tchva2(2)=tchvar(2)(1:nl1)//tchvac(2)(1:nl2)//'j'
        else
            tchva2(2)=tchvar(2)(1:nl1)//'+'//tchvac(2)(1:nl2)//'j'
        endif
!
        nl1=lxlgut(tchva2(1))
        nl2=lxlgut(tchva2(2))
        if (nl1 .ge. 48 .or. nl2 .ge. 48) call assert(.false.)
        if (nl1 .lt. 24) then
            if (nl2 .lt. 24) then
                if (llab) write(ific,2424)(lign2(i), i=1,nl)
                write(ific,6424)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else if (nl2.lt.36) then
                if (llab) write(ific,2436)(lign2(i), i=1,nl)
                write(ific,6436)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else
                if (llab) write(ific,2448)(lign2(i), i=1,nl)
                write(ific,6448)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            endif
        else if (nl1.lt.36) then
            if (nl2 .lt. 24) then
                if (llab) write(ific,3624)(lign2(i), i=1,nl)
                write(ific,7624)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else if (nl2.lt.36) then
                if (llab) write(ific,3636)(lign2(i), i=1,nl)
                write(ific,7636)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else
                if (llab) write(ific,3648)(lign2(i), i=1,nl)
                write(ific,7648)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            endif
        else
            if (nl2 .lt. 24) then
                if (llab) write(ific,4824)(lign2(i), i=1,nl)
                write(ific,8824)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else if (nl2.lt.36) then
                if (llab) write(ific,4836)(lign2(i), i=1,nl)
                write(ific,8836)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            else
                if (llab) write(ific,4848)(lign2(i), i=1,nl)
                write(ific,8848)testok,k120,k170,tchva2(1),tchva2(2),&
                tcherr(1),tcherr(2)
            endif
        endif
!
    endif
!
!     IF (TESTOK.EQ.'NOOK') CALL ABORT()
    1616 format(5x,6(1x,a16))
    5616 format(a4,1x,6(1x,a16))
    1624 format(5x,3(1x,a16),1(1x,a24),2(1x,a16))
    5624 format(a4,1x,3(1x,a16),1(1x,a24),2(1x,a16))
    2416 format(5x,2(1x,a16),1(1x,a24),3(1x,a16))
    6416 format(a4,1x,2(1x,a16),1(1x,a24),3(1x,a16))
    2424 format(5x,2(1x,a16),2(1x,a24),2(1x,a16))
    6424 format(a4,1x,2(1x,a16),2(1x,a24),2(1x,a16))
    2436 format(5x,2(1x,a16),1x,a24,1x,a36,2(1x,a16))
    6436 format(a4,1x,2(1x,a16),1x,a24,1x,a36,2(1x,a16))
    2448 format(5x,2(1x,a16),1x,a24,1x,a48,2(1x,a16))
    6448 format(a4,1x,2(1x,a16),1x,a24,1x,a48,2(1x,a16))
    3624 format(5x,2(1x,a16),1x,a36,1x,a24,2(1x,a16))
    7624 format(a4,1x,2(1x,a16),1x,a36,1x,a24,2(1x,a16))
    3636 format(5x,2(1x,a16),2(1x,a36),2(1x,a16))
    7636 format(a4,1x,2(1x,a16),2(1x,a36),2(1x,a16))
    3648 format(5x,2(1x,a16),1x,a36,1x,a48,2(1x,a16))
    7648 format(a4,1x,2(1x,a16),1x,a36,1x,a48,2(1x,a16))
    4824 format(5x,2(1x,a16),1x,a48,1x,a24,2(1x,a16))
    8824 format(a4,1x,2(1x,a16),1x,a48,1x,a24,2(1x,a16))
    4836 format(5x,2(1x,a16),1x,a48,1x,a36,2(1x,a16))
    8836 format(a4,1x,2(1x,a16),1x,a48,1x,a36,2(1x,a16))
    4848 format(5x,2(1x,a16),2(1x,a48),2(1x,a16))
    8848 format(a4,1x,2(1x,a16),2(1x,a48),2(1x,a16))
!
!
!
end subroutine
