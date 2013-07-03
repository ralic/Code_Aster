subroutine uterec(ndim, iflup, iflum, ino, mno,&
                  jno, nsomm, jac, term22, aux,&
                  ltheta, valthe, valunt, niv, ifm,&
                  xn, yn, zn, valhp, valhm,&
                  valtp, valtm, ityp, itemp, itemm,&
                  noe)
!-----------------------------------------------------------------------
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE: UTILITAIRE DE CALCUL DE L'ERREUR DUE A LA
!                         CONDITION D'ECHANGE. POUR AERER TE0003.
!
! IN NDIM : DIMENSION DU CALCUL
! IN IFLUP/M : ADRESSE JEVEUX DU FLUX +/-
! IN ITEMP/M : ADRESSE JEVEUX DE LA TEMPERATURE +/-
! IN INO/JNO/MNO : NUMERO DES NOEUDS DE L'ARETE (INO NUMERO FACE EN 3D)
! IN NSOMM  : NBRE DE SOMMETS DE L'ARETE OU DE LA FACE.
! IN JAC  : JACOBIEN
! IN LTHETA/VALTHE/UNT : PARAMETRES THETA METHODE
! IN NIV/IFM : PARAMETRE D'IMPRESSION
! IN ITYP : TYPE DE FACE
! IN XN/YN/ZN : COMPOSANTES DE LA NORMALE AUX POINTS D'INTEGRATION
! IN VALHP/M VALTP/M : PARAMETRES CL D'ECHANGE AUX INSTANTS + ET -
! IN NOE : TABLEAU NUMEROS DE NOEUDS PAR FACE ET PAR TYPE D'ELEMENT 3D
! OUT TERM22 : CONTRIBUTION DE L'ERREUR
! OUT AUX : TERME DE NORMALISATION
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       AUCUNE.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       24/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!       11/09/01 (OB): MODIF. REPARTITION ERREUR/NORMALISATION
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
! aslint: disable=W1504
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
    integer :: iflup, iflum, ndim, ino, mno, jno, nsomm, ityp, itemp, itemm
    integer :: noe(9, 6, 3), niv, ifm
    real(kind=8) :: jac(9), term22, aux, valthe, valunt, xn(9), yn(9), zn(9)
    real(kind=8) :: valhp(9), valhm(9), valtp(9), valtm(9)
    logical :: ltheta
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ij, in, iino, i1, i2
    real(kind=8) :: aux1, aux2, aux3, aux4, term23
!
! INIT.
    term22 = 0.d0
    aux = 0.d0
!
    if (ndim .eq. 2) then
!
! CAS 2D
! POINT DE DEPART: INO
        i1 = ino-1
        ij = iflup + i1*ndim
        i2 = itemp + i1
        aux1 = valthe*valhp(1)*valtp(1)
        aux2 = valthe*(zr(ij)*xn(1)+zr(ij+1)*yn(1)-valhp(1)*zr(i2))
        term23 = aux1 + aux2
        if (niv .eq. 2) write(ifm,*)' ZR INO P',aux1,aux2
        if (ltheta) then
            ij = ij+iflum-iflup
            i2 = itemm + i1
            aux3 = valunt*valhm(1)*valtm(1)
            aux4 = valunt*(zr(ij)*xn(1)+zr(ij+1)*yn(1)-valhm(1)*zr(i2) )
            aux1 = aux1 + aux3
            term23 = term23 + aux3 + aux4
            if (niv .eq. 2) write(ifm,*)' ZR INO M',aux3,aux4
        endif
        term22 = jac(1)*term23*term23
        aux = jac(1)*aux1*aux1
!
! POINT EXTREME: JNO
        i1 = jno-1
        ij = iflup + i1*ndim
        i2 = itemp + i1
        aux1 = valthe*valhp(2)*valtp(2)
        aux2 = valthe*(zr(ij)*xn(2)+zr(ij+1)*yn(2)-valhp(2)*zr(i2))
        term23 = aux1 + aux2
        if (niv .eq. 2) write(ifm,*)' ZR JNO P',aux1,aux2
        if (ltheta) then
            ij = ij+iflum-iflup
            i2 = itemm + i1
            aux3 = valunt*valhm(2)*valtm(2)
            aux4 = valunt*(zr(ij)*xn(2)+zr(ij+1)*yn(2)-valhm(2)*zr(i2) )
            aux1 = aux1 + aux3
            term23 = term23 + aux3 + aux4
            if (niv .eq. 2) write(ifm,*)' ZR JNO M',aux3,aux4
        endif
        term22 = term22 + jac(2)*term23*term23
        aux = aux + jac(2)*aux1*aux1
!
! POINT MILIEU SI NECESSAIRE: MNO
        if (nsomm .eq. 3) then
            i1 = mno-1
            ij = iflup + i1*ndim
            i2 = itemp + i1
            aux1 = valthe*valhp(3)*valtp(3)
            aux2 = valthe*(zr(ij)*xn(3)+zr(ij+1)*yn(3)-valhp(3)*zr(i2) )
            term23 = aux1 + aux2
            if (niv .eq. 2) write(ifm,*)' ZR MNO P',aux1,aux2
            if (ltheta) then
                ij = ij+iflum-iflup
                i2 = itemm + i1
                aux3 = valunt*valhm(3)*valtm(3)
                aux4 = valunt*(zr(ij)*xn(3)+zr(ij+1)*yn(3)-valhm(3)* zr(i2))
                aux1 = aux1 + aux3
                term23 = term23 + aux3 + aux4
                if (niv .eq. 2) write(ifm,*)' ZR MNO M',aux3,aux4
            endif
            term22 = term22 + jac(3)*term23*term23
            aux = aux + jac(3)*aux1*aux1
        endif
    else
!
! CAS 3D
        do 100 in = 1, nsomm
            iino = noe(in,ino,ityp)
            i1 = iino-1
            ij = iflup + i1*ndim
            i2 = itemp + i1
            aux1 = valthe*valhp(in)*valtp(in)
            aux2 = valthe*( zr(ij)*xn(in)+zr(ij+1)*yn(in)+ zr(ij+2)*zn( in)-valhp(in)*zr(i2) )
            term23 = aux1 + aux2
            if (niv .eq. 2) write(ifm,*)' ZR IINO P/IN ',aux1,aux2,in
            if (ltheta) then
                ij = ij+iflum-iflup
                i2 = itemm + i1
                aux3 = valunt*valhm(in)*valtm(in)
                aux4 = valunt*(zr(ij)*xn(in)+zr(ij+1)*yn(in)+ zr(ij+2) *zn(in)-valhm(in)*zr(i2))
                aux1 = aux1 + aux3
                term23 = term23 + aux3 + aux4
                if (niv .eq. 2) write(ifm,*)' ZR IINO M    ',aux3,aux4
            endif
            term22 = term22 + term23*term23*jac(in)
            aux = aux + aux1*aux1*jac(in)
100      continue
!
    endif
!
end subroutine
