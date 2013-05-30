subroutine te0045(option, nomte)
    implicit          none
    include 'jeveux.h'
!
    include 'asterfort/infdis.h'
    include 'asterfort/infted.h'
    include 'asterfort/jevech.h'
    include 'asterfort/matrot.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/ut2mlg.h'
    include 'asterfort/ut2plg.h'
    include 'asterfort/utpplg.h'
    include 'asterfort/utpslg.h'
    character(len=16) :: option, nomte
!     ------------------------------------------------------------------
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
!     CALCUL DES CARACTERISTIQUES DE STRUCTURES POUR LES ELEMENTS
!     DISCRETS : MECA_DIS_T_N         MECA_DIS_T_L
!                MECA_DIS_TR_N        MECA_DIS_TR_L
!                MECA_2D_DIS_T_N      MECA_2D_DIS_T_L
!                MECA_2D_DIS_TR_N     MECA_2D_DIS_TR_L
!     ------------------------------------------------------------------
! IN  : OPTION : NOM DE L'OPTION A CALCULER
! IN  : NOMTE  : NOM DU TYPE_ELEMENT
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
    character(len=8) :: k8bid
    character(len=16) :: ch16
    real(kind=8) :: mat1(78), mat2(144), zero, deux, trois, pgl(3, 3), r8bid
    integer :: infodi, irepm, nbterm, nno, nc, ndim, itype, i, i1, i2, i3
    integer :: lmass, lcoor, lorien, lcastr, ibid
    parameter     (zero=0.d0,deux=2.d0,trois=3.d0)
!     ------------------------------------------------------------------
!
    call infdis('SYMM', infodi, r8bid, k8bid)
    if (option .ne. 'MASS_INER') then
        ch16 = option
        call u2mesk('F', 'ELEMENTS2_47', 1, ch16)
    endif
!
!     ON VERIFIE QUE LES CARACTERISTIQUES ONT ETE AFFECTEES
!     LE CODE DU DISCRET
    call infdis('CODE', ibid, r8bid, nomte)
!     LE CODE STOKE DANS LA CARTE
    call infdis('TYDI', infodi, r8bid, k8bid)
    if (infodi .ne. ibid) then
        call u2mesk('F+', 'DISCRETS_25', 1, nomte)
        call infdis('DUMP', ibid, r8bid, 'F+')
    endif
!     DISCRET DE TYPE MASSE
    call infdis('DISM', infodi, r8bid, k8bid)
    if (infodi .eq. 0) then
        call u2mesk('A+', 'DISCRETS_26', 1, nomte)
        call infdis('DUMP', ibid, r8bid, 'A+')
    endif
!
!     MATRICES DE MASSE SYMETRIQUE ?
    call infdis('SYMM', infodi, r8bid, k8bid)
! --- INFORMATIONS SUR LES DISCRETS :
!        NBTERM   = NOMBRE DE COEFFICIENTS DANS K
!        NNO      = NOMBRE DE NOEUDS
!        NC       = NOMBRE DE COMPOSANTE PAR NOEUD
!        NDIM     = DIMENSION DE L'ELEMENT
!        ITYPE    = TYPE DE L'ELEMENT
    call infted(nomte, infodi, nbterm, nno, nc,&
                ndim, itype)
!      NEQ = NNO*NC
!
    call jevech('PCADISM', 'L', lmass)
    call jevech('PGEOMER', 'L', lcoor)
    call jevech('PCAORIE', 'L', lorien)
    call jevech('PMASSINE', 'E', lcastr)
    do 2 i = 0, 9
        zr(lcastr+i) = zero
 2  end do
    call matrot(zr(lorien), pgl)
!
!     REPERE DE LA MATRICE DE MASSE ?
    call infdis('REPM', irepm, r8bid, k8bid)
    if (infodi .eq. 1) then
        if (irepm .eq. 2) then
            if (ndim .eq. 3) then
                call utpslg(nno, nc, pgl, zr(lmass), mat1)
            else
                call ut2mlg(nno, nc, pgl, zr(lmass), mat1)
            endif
        else
            do 10 i = 1, nbterm
                mat1(i) = zr(lmass+i-1)
10          continue
        endif
    else if (infodi.eq.2) then
        if (irepm .eq. 2) then
            if (ndim .eq. 3) then
                call utpplg(nno, nc, pgl, zr(lmass), mat2)
            else
                call ut2plg(nno, nc, pgl, zr(lmass), mat2)
            endif
        else
            do 11 i = 1, nbterm
                mat2(i) = zr(lmass+i-1)
11          continue
        endif
    endif
!
!
!     ============ ELEMENT DE TYPE POI1 ============
    if (nomte .eq. 'MECA_DIS_T_N') then
!        --- CDG ---
        zr(lcastr+1) = zr(lcoor )
        zr(lcastr+2) = zr(lcoor+1)
        zr(lcastr+3) = zr(lcoor+2)
        if (infodi .eq. 1) then
!           --- MASSE ---
            do 12 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat1(i)
12          continue
            zr(lcastr) = zr(lcastr) + mat1(2) + mat1(4) + mat1(5)
            zr(lcastr) = zr(lcastr) / trois
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 13 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat2(i)
13          continue
            zr(lcastr) = zr(lcastr) / trois
        endif
!
    else if (nomte .eq. 'MECA_DIS_TR_N') then
!        --- CDG ---
        zr(lcastr+1) = zr(lcoor )
        zr(lcastr+2) = zr(lcoor+1)
        zr(lcastr+3) = zr(lcoor+2)
        if (infodi .eq. 1) then
!           --- MASSE ---
            do 22 i = 1, nc
                zr(lcastr) = zr(lcastr) + mat1(i)
22          continue
            zr(lcastr) = zr(lcastr) + mat1(2) + mat1(4) + mat1(5)
            zr(lcastr) = zr(lcastr) / trois
!           --- INERTIE ---
            zr(lcastr+4) = mat1(10)
            zr(lcastr+5) = mat1(15)
            zr(lcastr+6) = mat1(21)
            zr(lcastr+7) = mat1(14)
            zr(lcastr+8) = mat1(19)
            zr(lcastr+9) = mat1(20)
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 24 i = 1, nc
                zr(lcastr) = zr(lcastr) + mat2(i)
24          continue
            zr(lcastr) = zr(lcastr) / trois
!           --- INERTIE ---
            zr(lcastr+4) = mat2(22)
            zr(lcastr+5) = mat2(29)
            zr(lcastr+6) = mat2(36)
            zr(lcastr+7) = mat2(28)
            zr(lcastr+8) = mat2(34)
            zr(lcastr+9) = mat2(35)
        endif
!
    else if (nomte.eq. 'MECA_2D_DIS_T_N') then
!        --- CDG ---
        zr(lcastr+1) = zr(lcoor )
        zr(lcastr+2) = zr(lcoor+1)
        if (infodi .eq. 1) then
!           --- MASSE ---
            do 15 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat1(i)
15          continue
            zr(lcastr) = zr(lcastr) + mat1(2)
            zr(lcastr) = zr(lcastr) / deux
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 17 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat2(i)
17          continue
            zr(lcastr) = zr(lcastr) / deux
        endif
!
    else if (nomte .eq. 'MECA_2D_DIS_TR_N') then
!        --- CDG ---
        zr(lcastr+1) = zr(lcoor )
        zr(lcastr+2) = zr(lcoor+1)
        if (infodi .eq. 1) then
!           --- MASSE ---
            zr(lcastr) = zr(lcastr) + mat1(1) +deux*mat1(2) +mat1(3)
            zr(lcastr) = zr(lcastr) / deux
!           --- INERTIE ---
            zr(lcastr+3) = mat1(6)
        else if (infodi.eq.2) then
!           --- MASSE ---
            zr(lcastr) = zr(lcastr) / deux
!           --- INERTIE ---
            zr(lcastr+3) = mat2(9)
        endif
!
!     ============ ELEMENT DE TYPE SEG2 ============
    else if (nomte .eq. 'MECA_DIS_T_L') then
!        --- CDG ---
        zr(lcastr+1) = ( zr(lcoor ) + zr(lcoor+3) ) / deux
        zr(lcastr+2) = ( zr(lcoor+1) + zr(lcoor+4) ) / deux
        zr(lcastr+3) = ( zr(lcoor+2) + zr(lcoor+5) ) / deux
        if (infodi .eq. 1) then
!           --- MASSE ---
            do 32 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat1(i)
32          continue
            zr(lcastr) = zr(lcastr)+mat1(2)+mat1(4)+mat1(5)+mat1(7) + mat1(8)+mat1(9)+mat1(11)+ma&
                         &t1(12)+mat1(13) + mat1(14)+ mat1(16)+mat1(17)+mat1(18)+mat1(19) + mat1(&
                         &20)
            zr(lcastr) = zr(lcastr) / trois
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 33 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat2(i)
33          continue
            zr(lcastr) = zr(lcastr) / trois
        endif
!
    else if (nomte .eq. 'MECA_DIS_TR_L') then
!        --- CDG ---
        zr(lcastr+1) = ( zr(lcoor ) + zr(lcoor+3) ) / deux
        zr(lcastr+2) = ( zr(lcoor+1) + zr(lcoor+4) ) / deux
        zr(lcastr+3) = ( zr(lcoor+2) + zr(lcoor+5) ) / deux
        if (infodi .eq. 1) then
!           --- MASSE ---
            zr(lcastr) = mat1(2) + mat1(4) + mat1(5) + mat1(35) + mat1(43) + mat1(44)
            do 42 i = 1, 3
                i1 = 21 + i
                i2 = 28 + i
                i3 = 36 + i
                zr(lcastr) = zr(lcastr)+mat1(i1)+mat1(i2)+mat1(i3)
42          continue
            zr(lcastr) = deux * zr(lcastr)
            zr(lcastr) = zr(lcastr) + mat1(1) +mat1(3) +mat1(6) + mat1(28)+mat1(36)+mat1(45)
            zr(lcastr) = zr(lcastr) / trois
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 43 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat2(i)
43          continue
            zr(lcastr) = zr(lcastr) / trois
        endif
!
    else if (nomte .eq. 'MECA_2D_DIS_T_L') then
!        --- CDG ---
        zr(lcastr+1) = ( zr(lcoor ) + zr(lcoor+2) ) / deux
        zr(lcastr+2) = ( zr(lcoor+1) + zr(lcoor+3) ) / deux
        if (infodi .eq. 1) then
!        --- MASSE ---
            do 35 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat1(i)
35          continue
            zr(lcastr) = zr(lcastr)+mat1(2)+mat1(4)+mat1(5)+mat1(7) + mat1(8)+mat1(9)
            zr(lcastr) = zr(lcastr) / deux
        else if (infodi.eq.2) then
!           --- MASSE ---
            do 37 i = 1, nbterm
                zr(lcastr) = zr(lcastr) + mat2(i)
37          continue
            zr(lcastr) = zr(lcastr) / deux
        endif
!
    else if (nomte .eq. 'MECA_2D_DIS_TR_L') then
!        --- CDG ---
        zr(lcastr+1) = ( zr(lcoor ) + zr(lcoor+2) ) / deux
        zr(lcastr+2) = ( zr(lcoor+1) + zr(lcoor+3) ) / deux
        if (infodi .eq. 1) then
!           --- MASSE ---
            zr(lcastr) = mat1(2) + mat1(7) + mat1(8) + mat1(11) + mat1(12) + mat1(14)
            zr(lcastr) = deux * zr(lcastr)
            zr(lcastr) = zr(lcastr) + mat1(1) + mat1(3) + mat1(10) + mat1(15)
            zr(lcastr) = zr(lcastr) / deux
        else if (infodi.eq.2) then
!           --- MASSE ---
            zr(lcastr) = zr(lcastr) / deux
        endif
    endif
!
end subroutine
