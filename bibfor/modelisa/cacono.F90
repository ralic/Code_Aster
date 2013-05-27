subroutine cacono(noma, ndim, llist1, llist2, no1,&
                  no2, norm1, norm2, inoma)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/canorm.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/pacoor.h'
    include 'asterfort/panbno.h'
    include 'asterfort/u2mess.h'
    integer :: ndim, no1, no2
    character(len=8) :: noma
    character(len=24) :: llist1, llist2
    real(kind=8) :: norm1(*), norm2(*)
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
!     BUT : CALCUL DES NORMALES AUX NOEUDS NO1 ET NO2
!     APPARTENANT AUX LISTES DE MAILLES LLIST1 ET LLIST2
!     CUMUL DE CES NORMALES
!     CALCUL DU JEU : SOMME_SUR_I(X1(I)*N1(I)+X2(I)*N2(I))
!
! IN  NOMA    K8  : NOM DU MAILLAGE
! IN  NDIM    I   : DIMENSION DU MODELE (2 SI COORD_2D OU 3 SI COORD_3D)
! IN  LLIST1  K8  : NOM DE LA LISTE DE MAILLE ASSOCIEE AU NOEUD NO1
! IN  LLIST2  K8  : NOM DE LA LISTE DE MAILLE ASSOCIEE AU NOEUD NO2
! IN  NO1     I   : NOEUD NUMERO 1 (NUMERO ABSOLU)
! IN  NO2     I   : NOEUD NUMERO 2 (NUMERO ABSOLU)
!
! OUT NORM1   R8  : NORMALE AU NOEUD 1
! OUT NORM2   R8  : NORMALE AU NOEUD 2
! OUT INOMA   I   :  = 0    SI LE NOEUD 1 N'APPARTIENT PAS A LLIST1
!                        ET SI LE NOEUD 2 N'APPARTIENT PAS A LLIST2
!                    = 1    SINON
!                    =-1    SI LE NOEUD 1 APPARTIENT A UNE MAILLE POI1
!                    =-2    SI LE NOEUD 2 APPARTIENT A UNE MAILLE POI1
! ROUTINES APPELEES :
!     CANORM      PACOOR      PANBNO
!
!
!
    real(kind=8) :: vecnor(3), coor(27)
    integer :: nbma, numma, ipoi1, inoma, i, ilist, ipoi2, ima, ityp, imad, ino
    integer :: nbno, nbnott(3), inorm, j
! DEBUT ----------------------------------------------------------------
!
!     INORM = 0 POUR VECTEUR NORMAL DE NORME "SURFACE"
!     INORM = 1 POUR VECTEUR NORMAL UNITAIRE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    inorm = 1
    do 1 i = 1, 3
        norm1(i) = 0.0d0
        norm2(i) = 0.0d0
 1  end do
    call jeveuo(llist1, 'L', ilist)
    nbma = zi(ilist)
    inoma = 0
    ipoi1 = 0
    ipoi2 = 0
!
    do 100 ima = 1, nbma
        ityp = zi(ilist+2* (ima-1)+2)
        call panbno(ityp, nbnott)
        nbno = nbnott(1) + nbnott(2) + nbnott(3)
        numma = zi(ilist+2* (ima-1)+1)
        call jeveuo(jexnum(noma//'.CONNEX', numma), 'L', imad)
        do 110 ino = 1, nbno
            if (zi(imad-1+ino) .eq. no1) then
!           CAS D'UNE MAILLE POI1
                if (nbno .eq. 1) then
                    ipoi1 = 1
                    goto 100
                endif
                inoma = 1
                call pacoor(noma, numma, nbno, coor)
                call canorm(coor, vecnor, ndim, ityp, inorm)
                do 120 j = 1, 3
                    norm1(j) = norm1(j) + vecnor(j)
120              continue
                goto 100
!
            endif
!
110      continue
100  end do
!
    call jeveuo(llist2, 'L', ilist)
    nbma = zi(ilist)
    do 200 ima = 1, nbma
        ityp = zi(ilist+2* (ima-1)+2)
        call panbno(ityp, nbnott)
        nbno = nbnott(1) + nbnott(2) + nbnott(3)
        numma = zi(ilist+2* (ima-1)+1)
        call jeveuo(jexnum(noma//'.CONNEX', numma), 'L', imad)
        do 210 ino = 1, nbno
            if (zi(imad-1+ino) .eq. no2) then
!           CAS D'UNE MAILLE POI1
                if (nbno .eq. 1) then
                    ipoi2 = 1
                    goto 200
                endif
                inoma = 1
                call pacoor(noma, numma, nbno, coor)
                call canorm(coor, vecnor, ndim, ityp, inorm)
                do 220 j = 1, 3
                    norm2(j) = norm2(j) + vecnor(j)
220              continue
                goto 200
!
            endif
!
210      continue
200  end do
!
!     ON CHERCHE LE CAS OU NO2 APPARTIENT SEULEMENT A UNE MAILLE POI1
!     ON VERIFIE DANS CE CAS QUE NO1 N'APPARTIENT PAS A UNE MAILLE POI1
!
    if (ipoi1 .eq. 1) then
        inoma = -1
    else if (ipoi2.eq.1) then
        inoma = -2
    endif
    if ((ipoi1.eq.1) .and. (ipoi2.eq.1)) then
        call u2mess('F', 'MODELISA2_43')
    endif
!
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
