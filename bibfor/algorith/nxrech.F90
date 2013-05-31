subroutine nxrech(modele, mate, carele, charge, infoch,&
                  numedd, time, lonch, compor, vtempm,&
                  vtempp, vtempr, vtemp, vhydr, vhydrp,&
                  tmpchi, tmpchf, vec2nd, cnvabt, cnresi,&
                  rho, iterho, parmer, parmei)
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
! person_in_charge: Christophe-mmn.durand at edf.fr
!
! aslint: disable=W1504
    implicit none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/asasve.h'
    include 'asterfort/ascova.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/verstp.h'
    include 'asterfort/vethbt.h'
    integer :: parmei(2), lonch
    real(kind=8) :: parmer(2), rho
    character(len=24) :: modele, mate, carele, charge, infoch, numedd, time
    character(len=24) :: vtemp, vtempm, vtempp, vtempr, cnvabt, cnresi, vec2nd
    character(len=24) :: vhydr, vhydrp, compor, tmpchi, tmpchf
!
! ----------------------------------------------------------------------
!
! COMMANDE THER_NON_LINE : RECHERCHE LINEAIRE
! DANS LA DIRECTION DONNEE PAR NEWTON (ON CHERCHE RHO).
!
!
!
!
    integer :: i
    integer :: jtempm, jtempp, jtempr, j2nd, jvare, jbtla
    real(kind=8) :: rho0, rhot, f0, f1, rhomin, rhomax
    real(kind=8) :: rhof, ffinal
    real(kind=8) :: testm, r8bid
    character(len=24) :: vebtla, veresi, varesi, bidon, vabtla
    character(len=1) :: typres
    integer :: itrmax, k, iterho
    parameter (rhomin = -2.d0, rhomax = 2.d0)
    data typres        /'R'/
    data bidon         /'&&FOMULT.BIDON'/
    data veresi        /'&&VERESI           .RELR'/
    data vebtla        /'&&VETBTL           .RELR'/
!
!-----------------------------------------------------------------------
!
    call jemarq()
    varesi = '&&VARESI'
!
! --- RECUPERATION D'ADRESSES JEVEUX
!
    call jeveuo(vtempm(1:19)//'.VALE', 'E', jtempm)
    call jeveuo(vtempp(1:19)//'.VALE', 'E', jtempp)
    call jeveuo(vtempr(1:19)//'.VALE', 'E', jtempr)
    call jeveuo(vec2nd(1:19)//'.VALE', 'L', j2nd)
    call jeveuo(cnresi(1:19)//'.VALE', 'L', jvare)
    call jeveuo(cnvabt(1:19)//'.VALE', 'L', jbtla)
!
! --- RECHERCHE LINEAIRE (CALCUL DE RHO) SUR L'INCREMENT VTEMPP
!
    f0 = 0.d0
    do 330 i = 1, lonch
        f0 = f0 + zr(jtempp+i-1)*( zr(j2nd+i-1) - zr(jvare+i-1) - zr( jbtla+i-1) )
330  end do
!
    rho0 = 0.d0
    rho = 1.d0
    itrmax = parmei(2)+1
    do 20 iterho = 1, itrmax
        do 345 i = 1, lonch
            zr(jtempr+i-1) = zr(jtempm+i-1) + rho * zr(jtempp+i-1)
345      continue
!
! --- VECTEURS RESIDUS ELEMENTAIRES - CALCUL ET ASSEMBLAGE
!
        call verstp(modele, charge, infoch, carele, mate,&
                    time, compor, vtemp, vtempr, vhydr,&
                    vhydrp, tmpchi, tmpchf, veresi)
        call asasve(veresi, numedd, typres, varesi)
        call ascova('D', varesi, bidon, 'INST', r8bid,&
                    typres, cnresi)
        call jeveuo(cnresi(1:19)//'.VALE', 'L', jvare)
!
! --- BT LAMBDA - CALCUL ET ASSEMBLAGE
!
        call vethbt(modele, charge, infoch, carele, mate,&
                    vtempr, vebtla)
        call asasve(vebtla, numedd, typres, vabtla)
        call ascova('D', vabtla, bidon, 'INST', r8bid,&
                    typres, cnvabt)
        call jeveuo(cnvabt(1:19)//'.VALE', 'L', jbtla)
!
        f1 = 0.d0
        do 360 i = 1, lonch
            f1 = f1 + zr(jtempp+i-1) * ( zr(j2nd+i-1) - zr(jvare+i-1) - zr(jbtla+i-1) )
360      continue
        testm = 0.d0
        do 100 k = 1, lonch
            testm = max( testm, abs(zr(j2nd+k-1)-zr(jvare+k-1)-zr( jbtla+k-1)))
100      continue
        if (testm .lt. parmer(2)) goto 9999
!
        if (iterho .eq. 1) then
            ffinal = f1
            rhof = 1.d0
        endif
        if (abs(f1) .lt. abs(ffinal)) then
            ffinal=f1
            rhof=rho
        endif
        rhot=rho
        if (abs(f1-f0) .gt. r8prem()) then
            rho = -(f0*rhot-f1*rho0)/(f1-f0)
            if (rho .lt. rhomin) rho = rhomin
            if (rho .gt. rhomax) rho = rhomax
            if (abs(rho-rhot) .lt. 1.d-08) goto 40
        else
            goto 40
        endif
        rho0= rhot
        f0 = f1
20  continue
40  continue
    rho=rhof
    f1=ffinal
!
!-----------------------------------------------------------------------
9999  continue
    iterho = iterho - 1
    call jedema()
end subroutine
