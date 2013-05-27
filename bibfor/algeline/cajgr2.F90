subroutine cajgr2(igrap, vr, cocaj1, cocaj2)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! CALCUL DES COEFFICIENTS ADIMENSIONNELS DE FORCE D'AMORTISSEMENT
! GRAPPE2
!-----------------------------------------------------------------------
!  IN : IGRAP  : INDICE CARACTERISTIQUE DE LA CONFIGURATION
!                EXPERIMENTALE DE REFERENCE
!  IN : VR     : VITESSE REDUITE
! OUT : COCAJ1 : COEFFICIENT ADIMENSIONNEL DE FORCE D'AMORTISSEMENT
!                POUR UN MOUVEMENT DE TRANSLATION
! OUT : COCAJ2 : COEFFICIENT ADIMENSIONNEL DE FORCE D'AMORTISSEMENT
!                POUR UN MOUVEMENT DE ROTATION
!-----------------------------------------------------------------------
!     UN COMMON AJOUTE POUR RESORBER UNE GLUTE ANTIQUE (VOIR HISTOR):
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/ulopen.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: typflu
    common  / kop144 / typflu
!
    integer :: igrap
    real(kind=8) :: vr, cocaj1, cocaj2
!
    integer :: ncamax, nbomax, unit, nbloc, iflag
    real(kind=8) :: coeca1(10, 20, 11), coeca2(10, 20, 11)
    real(kind=8) :: boca1(10, 20), boca2(10, 20), borne1(10, 20)
    real(kind=8) :: zero, coef1(10, 20, 11), coef2(10, 20, 11), borne2(10, 20)
    character(len=24) :: nom1, nom2
    save          borne1, coeca1, borne2, coeca2
! ----------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: i, iret, iunit, j, k, l, nb1
    integer :: nb2, nbmax
    real(kind=8) :: vr2, vr3
!-----------------------------------------------------------------------
    call jemarq()
    ncamax = 11
    nbomax = 20
    nbmax = 10
    zero = 0.0d0
!
    nom1 = '&&CAJGR2.FLAG'
    nom2 = typflu//'.UNIT_GRAPPES'
!
! --- ON TESTE L'EXISTENCE DU VECTEUR DES COEFFICIENTS
!     ===============================================
    call jeexin(nom1, iret)
    if (iret .eq. 0) then
!
! --- LECTURE DU FICHIER DE DONNEES
!     =============================
        call jeveuo(nom2, 'L', iunit)
        unit = zi(iunit-1+1)
        call ulopen(unit, ' ', ' ', 'NEW', 'O')
!
! ---    BLOC D'INITIALISATION
        do 10 i = 1, nbmax
            do 20 j = 1, nbomax
                boca1(i,j) = zero
                boca2(i,j) = zero
                borne1(i,j) = zero
                borne2(i,j) = zero
                do 30 k = 1, ncamax
                    coeca1(i,j,k) = zero
                    coeca2(i,j,k) = zero
                    coef1(i,j,k) = zero
                    coef2(i,j,k) = zero
30              continue
20          continue
10      continue
!
        read (unit,*) nbloc
        do 40 l = 1, nbloc
            read (unit,*) nb1
            if (nb1 .ne. 0) then
                read (unit,*) (boca1(l,i),i = 1,nb1)
            endif
            do 50 i = 1, nb1+1
                read (unit,*) (coef1(l,i,j),j = 1,ncamax)
50          continue
            read (unit,*) nb2
            if (nb2 .ne. 0) then
                read (unit,*) (boca2(l,i),i = 1,nb2)
            endif
            do 60 i = 1, nb2+1
                read (unit,*) (coef2(l,i,j),j = 1,ncamax)
60          continue
            read (unit,*)
            do 70 i = 1, nbmax
                do 80 j = 1, nbomax
                    borne1(i,j) = boca1(i,j)
                    borne2(i,j) = boca2(i,j)
                    do 90 k = 1, ncamax
                        coeca1(i,j,k) = coef1(i,j,k)
                        coeca2(i,j,k) = coef2(i,j,k)
90                  continue
80              continue
70          continue
            call jedetr(nom1)
            call wkvect(nom1, 'V V I', 1, iflag)
            zi(iflag-1+1) = 1
40      continue
    endif
!
!-----1.CONFIG. ECOULEMENT ASCENDANT TIGE DE COMMANDE CENTREE
!
    if (igrap .eq. 1) then
!
        if (vr .lt. borne1(1,1)) then
            cocaj1 = coeca1(1,1,8) + coeca1(1,1,9)*vr
        else
            cocaj1 = coeca1(1,2,8) + coeca1(1,2,9)*vr
        endif
!
        cocaj2 = coeca2(1,1,8) + coeca2(1,1,9)*vr
!
!-----2.CONFIG. ECOULEMENT ASCENDANT TIGE DE COMMANDE EXCENTREE
!
    else if (igrap.eq.2) then
!
        cocaj1 = coeca1(2,1,8) + coeca1(2,1,9)*vr
        cocaj2 = coeca2(2,1,8) + coeca2(2,1,9)*vr
!
!-----3.CONFIG. ECOULEMENT DESCENDANT TIGE DE COMMANDE CENTREE
!
    else if (igrap.eq.3) then
!
        if (vr .lt. borne1(3,1)) then
            vr2 = vr*vr
            vr3 = vr2*vr
            cocaj1 = coeca1(3,1,8) + coeca1(3,1,9)*vr + coeca1(3,1,10) *vr2 + coeca1(3,1,11)*vr3
        else if (vr .lt. borne1(3,2)) then
            cocaj1 = coeca1(3,2,8)
        else
            cocaj1 = coeca1(3,3,8) + coeca1(3,3,9)*vr
        endif
!
        if (vr .lt. borne2(3,1)) then
            cocaj2 = coeca2(3,1,8) + coeca2(3,1,9)*vr
        else if (vr .lt. borne2(3,2)) then
            vr2 = vr*vr
            cocaj2 = coeca2(3,2,8) + coeca2(3,2,9)*vr + coeca2(3,2,10) *vr2
        else
            cocaj2 = coeca2(3,3,8) + coeca2(3,3,9)*vr
        endif
!
!-----4.CONFIG. ECOULEMENT DESCENDANT TIGE DE COMMANDE EXCENTREE
!
    else
!
        if (vr .lt. borne1(4,1)) then
            vr2 = vr*vr
            vr3 = vr2*vr
            cocaj1 = coeca1(4,1,8) + coeca1(4,1,9)*vr + coeca1(4,1,10) *vr2 + coeca1(4,1,11)*vr3
        else if (vr .lt. borne1(4,2)) then
            vr2 = vr*vr
            vr3 = vr2*vr
            cocaj1 = coeca1(4,2,8) + coeca1(4,2,9)*vr + coeca1(4,2,10) *vr2 + coeca1(4,2,11)*vr3
        else
            cocaj1 = coeca1(4,3,8) + coeca1(4,3,9)*vr
        endif
!
        if (vr .lt. borne2(4,1)) then
            cocaj2 = coeca2(4,1,8) + coeca2(4,1,9)*vr
        else if (vr .lt. borne2(4,2)) then
            vr2 = vr*vr
            cocaj2 = coeca2(4,2,8) + coeca2(4,2,9)*vr + coeca2(4,2,10) *vr2
        else
            cocaj2 = coeca2(4,3,8) + coeca2(4,3,9)*vr
        endif
!
    endif
!
!     FERMETURE DU FICHIER
    if (iret .eq. 0) call ulopen(-unit, ' ', ' ', ' ', ' ')
    call jedema()
end subroutine
