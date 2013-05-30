subroutine dichoc(nbt, neq, nno, nc, icodma,&
                  dul, utl, xg, pgl, klv,&
                  duly, dvl, dpe, dve, force,&
                  varmo, varpl, dimele)
! ----------------------------------------------------------------------
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvala.h'
    include 'asterfort/ut2vgl.h'
    include 'asterfort/utpvgl.h'
    include 'asterfort/vdiff.h'
    include 'blas/dcopy.h'
    integer :: nbt, neq, nno, nc, icodma, dimele
    real(kind=8) :: dul(neq), utl(neq), dvl(neq)
    real(kind=8) :: dpe(neq), dve(neq)
    real(kind=8) :: klv(nbt), duly, xg(6), pgl(3, 3)
    real(kind=8) :: varmo(8), varpl(8), force(3)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
!     RELATION DE COMPORTEMENT "DIS_CHOC"
!
! ----------------------------------------------------------------------
!
! IN  :  NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!        NEQ    : NOMBRE DE DDL DE L'ELEMENT
!        ICODMA : ADRESSE DU MATERIAU CODE
!        DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
!        UTL    : DEPLACEMENT COURANT REPERE LOCAL
!        DIMELE : DIMENSION DE L'ELEMENT
!
! OUT :  KLV    : MATRICE TANGENTE
!        DULY   :
!
! =============== DECLARATION DES VARIABLES LOCALES ====================
!
    integer :: nbre1, nbpar, n
    parameter     (nbre1=8)
    real(kind=8) :: valre1(nbre1)
    integer :: codre1(nbre1)
    character(len=8) :: nompar, nomre1(nbre1)
!
    real(kind=8) :: xl(6), xd(3), dirl(6), zero, rignor, rigtan
    real(kind=8) :: coulom, dist12, utot, vit2, vit3, depx, depy, depz, psca
    real(kind=8) :: vitt, valpar, vity, vitz, fort, dist0, kty
!
    data nomre1 /'RIGI_NOR','RIGI_TAN','AMOR_NOR','AMOR_TAN'&
     &            ,'COULOMB','DIST_1','DIST_2','JEU'/
! ----------------------------------------------------------------------
!
! --- DEFINITION DES PARAMETRES
    zero = 0.d0
    call r8inir(6, zero, xl, 1)
    call r8inir(6, zero, dirl, 1)
    call r8inir(3, zero, xd, 1)
!     COORDONNEES DANS LE REPERE LOCAL
    if (dimele .eq. 3) then
        call utpvgl(nno, 3, pgl, xg, xl)
    else if (dimele.eq.2) then
        call ut2vgl(nno, 2, pgl, xg, xl)
    endif
!
!     ELEMENT A 2 NOEUDS
    if (nno .eq. 2) then
        nbpar = 0
        nompar = ' '
        valpar = 0.d0
        call r8inir(nbre1, zero, valre1, 1)
! ---    CARACTERISTIQUES DU MATERIAU
!        SI MOT_CLE RIGI_NOR ==> RIGNOR = VALRE1(1)
!        SINON               ==> RIGNOR = KLV(1)
        call rcvala(icodma, ' ', 'DIS_CONTACT', nbpar, nompar,&
                    valpar, nbre1, nomre1, valre1, codre1,&
                    0)
        if (codre1(1) .eq. 0) then
            rignor = valre1(1)
        else
            rignor = klv(1)
        endif
        rigtan = valre1(2)
!        AMONOR = VALRE1(3)
!        AMOTAN = VALRE1(4)
        coulom = valre1(5)
        dist12 = valre1(6)+valre1(7)
!        DANS L'AXE DU DISCRET
        duly = dul(1+nc)-dul(1)
        utot = utl(1+nc)-utl(1)
!        VITESSE TANGENTE
        vit2 = dvl(2+nc)-dvl(2)
        vit3 = zero
        if (dimele .eq. 3) vit3 = dvl(3+nc)-dvl(3)
!        LONGUEUR DU DISCRET
        call vdiff(dimele, xl(1+dimele), xl(1), xd)
        call dcopy(dimele, dpe(1), 1, dirl, 1)
        call dcopy(dimele, dpe(1+nc), 1, dirl(4), 1)
        depx = xd(1) - dist12 + utot + dirl(4) - dirl(1)
        depx = depx - r8prem()
        depy = xd(2) + utl(2+nc) - utl(2) + dirl(5) - dirl(2)
        depz = zero
        if (dimele .eq. 3) then
            depz = xd(3) + utl(3+nc) - utl(3) + dirl(6) - dirl(3)
        endif
        call dcopy(dimele, dve(1), 1, dirl, 1)
        call dcopy(dimele, dve(1+nc), 1, dirl(4), 1)
!        VITESSE TANGENTE
        vity = vit2 + dirl(5) - dirl(2)
        vitz = zero
        if (dimele .eq. 3) then
            vitz = vit3 + dirl(6) - dirl(3)
        endif
        if (depx .le. zero) then
            kty = rignor
            force(1) = rignor*depx
            if (force(1) .gt. zero) force(1) = zero
            psca = varmo(5)*vity + varmo(6)*vitz
            if (psca .ge. zero .and. varmo(7) .eq. 1.d0) then
                vitt = (vity**2 + vitz**2)**0.5d0
                force(2) = zero
                force(3) = zero
                if (vitt .ne. zero) then
                    force(2) = -coulom*force(1)*vity/vitt
                    force(3) = -coulom*force(1)*vitz/vitt
                endif
                varpl(7) = 1.d0
            else
                force(2) = rigtan*(depy-varmo(1)) + varmo(5)
                force(3) = rigtan*(depz-varmo(2)) + varmo(6)
                varpl(7) = zero
                fort = (force(2)**2 + force(3)**2)**0.5d0
                if (fort .gt. abs(coulom*force(1))) then
                    vitt = (vity**2 + vitz**2)**0.5d0
                    force(2) = zero
                    force(3) = zero
                    if (vitt .ne. zero) then
                        force(2) = -coulom*force(1)*vity/vitt
                        force(3) = -coulom*force(1)*vitz/vitt
                        varpl(7) = 1.d0
                    endif
                endif
            endif
            varpl(5) = force(2)
            varpl(6) = force(3)
            force(2) = force(2) + klv(3)*(utl(2+nc)-utl(2))
            if (dimele .eq. 3) then
                force(3) = force(3) + klv(6)*(utl(3+nc)-utl(3))
            endif
            if (dimele .eq. 3) then
                if (neq .eq. 6) then
                    klv( 1) = kty
                    klv( 7) = -kty
                    klv(10) = kty
                else if (neq.eq.12) then
                    klv( 1) = kty
                    klv(22) = -kty
                    klv(28) = kty
                endif
            else
                if (neq .eq. 4) then
                    klv( 1) = kty
                    klv( 4) = -kty
                    klv( 6) = kty
                else if (neq.eq.6) then
                    klv( 1) = kty
                    klv( 7) = -kty
                    klv(10) = kty
                endif
            endif
        else
            kty = zero
            force(1) = zero
            force(2) = zero
            force(3) = zero
            varpl(5) = zero
            varpl(6) = zero
            varpl(7) = zero
            do 10 n = 1, nbt
                klv(n)= zero
10          continue
        endif
        varpl(1) = depy
        varpl(2) = depz
        varpl(3) = vity
        varpl(4) = vitz
        varpl(8) = depx
!
!     ELEMENT A 1 NOEUD
    else
        nbpar = 0
        nompar = ' '
        valpar = 0.d0
        call r8inir(nbre1, zero, valre1, 1)
! ---    CARACTERISTIQUES DU MATERIAU
!        SI MOT_CLE RIGI_NOR ==> RIGNOR = VALRE1(1)
!        SINON               ==> RIGNOR = KLV(1)
        call rcvala(icodma, ' ', 'DIS_CONTACT', nbpar, nompar,&
                    valpar, nbre1, nomre1, valre1, codre1,&
                    0)
        if (codre1(1) .eq. 0) then
            rignor = valre1(1)
        else
            rignor = klv(1)
        endif
        rigtan = valre1(2)
!        AMONOR = VALRE1(3)
!        AMOTAN = VALRE1(4)
        coulom = valre1(5)
        dist12 = valre1(6)
        dist0 = valre1(8)
!        DANS L'AXE DU DISCRET
        duly = dul(1)
!        VITESSE TANGENTE
        vit2 = dvl(2)
        vit3 = zero
        if (dimele .eq. 3) vit3 = dvl(3)
!        LONGUEUR DU DISCRET
        call dcopy(dimele, dpe(1), 1, dirl, 1)
        depx = utl(1) + dist12 + dirl(1) - dist0
        depy = utl(2) + dirl(2)
        depz = zero
        if (dimele .eq. 3) depz = utl(3) + dirl(3)
        call dcopy(dimele, dve(1), 1, dirl, 1)
!        VITESSE TANGENTE
        vity = vit2 + dirl(2)
        vitz = zero
        if (dimele .eq. 3) vitz = vit3 + dirl(3)
        if (depx .ge. zero) then
            kty = rignor
            force(1) = rignor*depx
            if (force(1) .lt. zero) force(1) = zero
            psca = varmo(5)*vity + varmo(6)*vitz
            if (psca .ge. zero .and. varmo(7) .eq. 1.d0) then
                vitt = (vity**2 + vitz**2)**0.5d0
                force(2) = zero
                force(3) = zero
                if (vitt .ne. zero) then
                    force(2) = coulom*force(1)*vity/vitt
                    force(3) = coulom*force(1)*vitz/vitt
                endif
                varpl(7) = 1.d0
            else
                force(2) = rigtan*(depy-varmo(1)) + varmo(5)
                force(3) = rigtan*(depz-varmo(2)) + varmo(6)
                varpl(7) = zero
                fort = (force(2)**2 + force(3)**2)**0.5d0
                if (fort .gt. abs(coulom*force(1))) then
                    vitt = (vity**2 + vitz**2)**0.5d0
                    force(2) = zero
                    force(3) = zero
                    if (vitt .ne. zero) then
                        force(2) = coulom*force(1)*vity/vitt
                        force(3) = coulom*force(1)*vitz/vitt
                        varpl(7) = 1.d0
                    endif
                endif
            endif
            varpl(5) = force(2)
            varpl(6) = force(3)
            force(2) = force(2) + klv(3)*utl(2)
            if (dimele .eq. 3) force(3) = force(3) + klv(6)*utl(3)
            klv(1) = kty
        else
            kty = zero
            force(1) = zero
            force(2) = zero
            force(3) = zero
            varpl(5) = zero
            varpl(6) = zero
            varpl(7) = zero
            do 20 n = 1, nbt
                klv(n)= zero
20          continue
        endif
        varpl(1) = depy
        varpl(2) = depz
        varpl(3) = vity
        varpl(4) = vitz
        varpl(8) = depx
    endif
!
end subroutine
