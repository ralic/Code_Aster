subroutine ndgrot(sddyna, valinc, solalg, deldet, theta1,&
                  theta2, iran)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/marota.h'
    include 'asterfort/ndynre.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/promat.h'
    include 'asterfort/proqua.h'
    include 'asterfort/quavro.h'
    include 'asterfort/transp.h'
    include 'asterfort/vroqua.h'
    real(kind=8) :: theta2(3), theta1(3), deldet(3)
    character(len=19) :: sddyna
    character(len=19) :: solalg(*), valinc(*)
    integer :: iran(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - UTILITAIRE - DYNAMIQUE)
!
! MISE A JOUR DES VITESSES/ACCELERATIONS EN GRANDES ROTATIONS
! POUR POU_D_GD
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  THETA2 : VALEUR DE LA ROTATION PRECEDENTE
! IN  DELDET : INCREMENT DE ROTATION
! IN  IRAN   : NUMEROS ABSOLUS D'EQUATION DES DDL DE ROTATION DANS LES
!                 CHAM_NO
!
!
!
!
    character(len=19) :: depmoi, depplu, vitplu, accplu
    integer :: jdepm, jdepp, jvitp, jaccp
    character(len=19) :: depkm1, vitkm1, acckm1, romk, romkm1
    integer :: jdepkm, jvitkm, jacckm, jromk, jromkm
    integer :: ic
    real(kind=8) :: quapro(4), quarot(4), delqua(4)
    real(kind=8) :: qim(3), qikm1(3), qik(3), omkm1(3), ompkm1(3), delrot(3)
    real(kind=8) :: vect1(3), vect2(3), vect3(3), vect4(3), rotm(3, 3)
    real(kind=8) :: rotkm(3, 3), rotk(3, 3), rotmt(3, 3), rotkmt(3, 3)
    real(kind=8) :: coevit, coeacc
    character(len=19) :: depdel
    integer :: jdepde
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- COEFFICIENTS
!
    coevit = ndynre(sddyna,'COEF_VITE')
    coeacc = ndynre(sddyna,'COEF_ACCE')
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'DEPKM1', depkm1)
    call nmchex(valinc, 'VALINC', 'VITKM1', vitkm1)
    call nmchex(valinc, 'VALINC', 'ACCKM1', acckm1)
    call nmchex(valinc, 'VALINC', 'ROMKM1', romkm1)
    call nmchex(valinc, 'VALINC', 'ROMK  ', romk)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- RECUPERATION DES ADRESSES
!
    call jeveuo(depmoi(1:19)//'.VALE', 'L', jdepm)
    call jeveuo(depplu(1:19)//'.VALE', 'E', jdepp)
    call jeveuo(depdel(1:19)//'.VALE', 'E', jdepde)
    call jeveuo(vitplu(1:19)//'.VALE', 'E', jvitp)
    call jeveuo(accplu(1:19)//'.VALE', 'E', jaccp)
    call jeveuo(depkm1(1:19)//'.VALE', 'L', jdepkm)
    call jeveuo(vitkm1(1:19)//'.VALE', 'L', jvitkm)
    call jeveuo(acckm1(1:19)//'.VALE', 'L', jacckm)
    call jeveuo(romkm1(1:19)//'.VALE', 'L', jromkm)
    call jeveuo(romk(1:19) //'.VALE', 'E', jromk)
!
! --- QUATERNION DE L'INCREMENT DE ROTATION
!
    call vroqua(deldet, delqua)
!
! --- QUATERNION DE LA ROTATION PRECEDENTE
!
    call vroqua(theta1, quarot)
!
! --- CALCUL DE LA NOUVELLE ROTATION
!
    call proqua(delqua, quarot, quapro)
    call quavro(quapro, theta1)
!
! --- MISE A JOUR DES DEPLACEMENTS
!
    do 14 ic = 1, 3
        zr(jdepp+iran(ic) -1) = theta1(ic)
        zr(jdepde+iran(ic)-1) = theta1(ic)
14  end do
!
! --- QUATERNION DE LA ROTATION PRECEDENTE
!
    call vroqua(theta2, quarot)
!
! --- CALCUL DE LA NOUVELLE ROTATION
!
    call proqua(delqua, quarot, quapro)
    call quavro(quapro, theta2)
!
! --- MISE A JOUR DE LA ROTATION
!
    do 15 ic = 1, 3
        zr(jromk+iran(ic) -1) = theta2(ic)
15  end do
!
! --- CALCUL DES INCREMENTS DE ROTATION
!
    do 16 ic = 1, 3
        qim (ic) = zr(jdepm+iran(ic) -1)
        qikm1 (ic) = zr(jdepkm+iran(ic)-1)
        qik (ic) = zr(jdepp+iran(ic) -1)
        omkm1 (ic) = zr(jvitkm+iran(ic)-1)
        ompkm1(ic) = zr(jacckm+iran(ic)-1)
16  end do
!
! --- CALCUL DE L'INCREMENT DE ROTATION TOTALE
!
    do 17 ic = 1, 3
        delrot(ic) = zr(jromk+iran(ic) -1) - zr(jromkm+iran(ic)-1)
17  end do
!
! --- CALCUL DES MATRICES DE ROTATION
!
    call marota(qim, rotm)
    call marota(qikm1, rotkm)
    call marota(qik, rotk)
    call transp(rotm, 3, 3, 3, rotmt,&
                3)
    call transp(rotkm, 3, 3, 3, rotkmt,&
                3)
!
! --- CALCUL DE LA VITESSE ANGULAIRE
!
    call promat(rotmt, 3, 3, 3, delrot,&
                3, 3, 1, vect3)
    call promat(rotk, 3, 3, 3, vect3,&
                3, 3, 1, vect2)
    call promat(rotkmt, 3, 3, 3, omkm1,&
                3, 3, 1, vect3)
    call promat(rotk, 3, 3, 3, vect3,&
                3, 3, 1, vect1)
    do 18 ic = 1, 3
        zr(jvitp+iran(ic)-1) = vect1(ic) + coevit*vect2(ic)
18  end do
!
! --- CALCUL DE L'ACCELERATION ANGULAIRE
!
    call promat(rotkmt, 3, 3, 3, ompkm1,&
                3, 3, 1, vect4)
    call promat(rotk, 3, 3, 3, vect4,&
                3, 3, 1, vect3)
    do 19 ic = 1, 3
        zr(jaccp+iran(ic)-1) = vect3(ic) + coeacc*vect2(ic)
19  end do
!
    call jedema()
end subroutine
