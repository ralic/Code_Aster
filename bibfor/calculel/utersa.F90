subroutine utersa(ndim, iflup, iflum, ino, mno,&
                  jno, ivois, ma, iel, nbnv,&
                  nbsv, iavalp, iavalm, nsomm, jac,&
                  ltheta, valthe, valunt, niv, ifm,&
                  ityp, xn, yn, zn, term22,&
                  aux, jad, jadv, noe)
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
!    - FONCTION REALISEE: UTILITAIRE DE CALCUL DE L'ERREUR DUE AU TERME
!                         DE SAUT. POUR AERER TE0003.
!
! IN NDIM : DIMENSION DU CALCUL
! IN IFLUP/M : ADRESSE JEVEUX DU FLUX +/-
! IN INO/JNO/MNO : NUMERO DES NOEUDS DE L'ARETE (INO NUMERO FACE EN 3D)
! IN MA : MAILLAGE
! IN IVOIS : ADRESSE JEVEUX DE VOISIN
! IN IEL : INDICE DE LA MAILLE VOISINE DANS LE IGREL.
! IN NBNV : NOMBRE DE NOEUDS DE LA MAILLE VOISINE.
! IN NBSV : CODE DE LA MAILLE VOISINE (2D POUR MAILLE SYMETRIQUE).
! IN IAVALP/M : ADRESSE JEVEUX DES FLUX + ET - DU VOISIN
! IN NSOMM  : NBRE DE SOMMETS DE L'ARETE OU DE LA FACE.
! IN JAC  : JACOBIEN
! IN LTHETA/VALTHE/UNT : PARAMETRES THETA METHODE
! IN NIV/IFM : PARAMETRES D'IMPRESSION
! IN ITYP : TYPE DE FACE (EN 3D)
! IN XN/YN/ZN : COMPOSANTES DE LA NORMALE AUX POINTS D'INTEGRATION
! IN NOE : TABLEAU NUMEROS DE NOEUDS PAR FACE ET PAR TYPE D'ELEMENT 3D
! OUT TERM22 : CONTRIBUTION DE L'ERREUR
! OUT AUX : TERME DE NORMALISATION
! OUT JAD/JADV : ADRESSE JEVEUX DE LA MAILLE ET DE SA VOISINE
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       JEVEUX: JEVEUO,JEXNUM.
!       TRI: INDIIS.
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
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/indiis.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: iflup, iflum, ndim, ino, mno, jno, ivois, nsomm, ityp, iel, nbnv
    integer :: iavalp, iavalm, nbsv, jad, jadv, noe(9, 6, 3), niv, ifm
    real(kind=8) :: jac(9), term22, aux, valthe, valunt, xn(9), yn(9), zn(9)
    character(len=8) :: ma
    aster_logical :: ltheta
!
!
! DECLARATION VARIABLES LOCALES
    integer :: ij, in, iino, i1, inov0, jnov0, mnov, ncher, inov, jnov, iinov
    real(kind=8) :: aux1, aux2, aux3, aux4, term23
!
! INIT.
    term22 = 0.d0
    aux = 0.d0
    call jeveuo(jexnum(ma//'.CONNEX', zi(ivois)), 'L', jad)
    call jeveuo(jexnum(ma//'.CONNEX', zi(ivois+ino)), 'L', jadv)
!
    if (ndim .eq. 2) then
! CAS 2D
!
! NUMERO LOCALE (INOV) DANS LA MAILLE VOISINE DU PREMIER NOEUD (DE
! NUMERO GLOBALE NCHER) DE NUM LOCAL INO ET COMMUN AUX DEUX EFS.
        ncher = zi(jad-1+ino)
        inov = indiis(zi(jadv),ncher,1,nbnv)
        if (niv .eq. 2) write(ifm,*)'INOV LOCAL/GLOBAL',inov,ncher
! NUMERO LOCALE (JNOV)  DU NOEUD VOISIN DE JNO
        ncher = zi(jad-1+jno)
        jnov = indiis(zi(jadv),ncher,1,nbnv)
        if (niv .eq. 2) write(ifm,*)'JNOV LOCAL/GLOBAL',jnov,ncher
!
! POINT DE DEPART: INO
        ij = iflup+(ino-1)*ndim
        i1 = iavalp+ndim*((iel-1)*nbnv+inov-1)
        aux2 = -valthe*(zr(ij)*xn(1)+zr(ij+1)*yn(1))
        aux1 = valthe*(zr(i1)*xn(1)+zr(i1+1)*yn(1))
        term23 = aux1 + aux2
        if (niv .eq. 2) write(ifm,*)' ZR INO P',aux1,aux2
        if (ltheta) then
            ij = ij+iflum-iflup
            i1 = i1+iavalm-iavalp
            aux4 = -valunt*(zr(ij)*xn(1)+zr(ij+1)*yn(1))
            aux3 = valunt*(zr(i1)*xn(1)+zr(i1+1)*yn(1))
            aux1 = aux1 + aux3
            aux2 = aux2 + aux4
            term23 = term23 + aux3 + aux4
            if (niv .eq. 2) write(ifm,*)' ZR INO M',aux3,aux4
        endif
        term22 = jac(1)*term23*term23
        aux1 = (aux1-aux2)*0.5d0
        aux = jac(1)*aux1*aux1
!
! POINT EXTREME: JNO
        ij = iflup+(jno-1)*ndim
        i1 = iavalp+ndim*((iel-1)*nbnv+jnov-1)
        aux2 = -valthe*(zr(ij)*xn(2)+zr(ij+1)*yn(2))
        aux1 = valthe*(zr(i1)*xn(2)+zr(i1+1)*yn(2))
        term23 = aux1 + aux2
        if (niv .eq. 2) write(ifm,*)' ZR JNO P',aux1,aux2
        if (ltheta) then
            ij = ij+iflum-iflup
            i1 = i1+iavalm-iavalp
            aux4 = -valunt*(zr(ij)*xn(2)+zr(ij+1)*yn(2))
            aux3 = valunt*(zr(i1)*xn(2)+zr(i1+1)*yn(2))
            aux1 = aux1 + aux3
            aux2 = aux2 + aux4
            term23 = term23 + aux3 + aux4
            if (niv .eq. 2) write(ifm,*)' ZR JNO M',aux3,aux4
        endif
        term22 = term22 + jac(2)*term23*term23
        aux1 = (aux1-aux2)*0.5d0
        aux = aux + jac(2)*aux1*aux1
!
! POINT MILIEU SI NECESSAIRE: MNO
        if (nsomm .eq. 3) then
! TESTS POUR CALCULER MNOV AFIN DE TRAITER LES MAILLES VOISINES
! COMPORTANT DES ORIENTATIONS LOCALES IDENTIQUES (MAILLAGE SYMETRISE)
            if ((inov.eq.nbsv) .and. (jnov.eq.1)) then
                inov0 = 0
            else
                inov0 = inov
            endif
            if ((jnov.eq.nbsv) .and. (inov.eq.1)) then
                jnov0 = 0
            else
                jnov0 = jnov
            endif
            if (inov0 .lt. jnov0) then
                mnov = inov + nbsv
            else
                mnov = jnov + nbsv
            endif
            ij = iflup+(mno-1)*ndim
            i1 = iavalp+ndim*((iel-1)*nbnv+mnov-1)
            aux2 = -valthe*(zr(ij)*xn(3)+zr(ij+1)*yn(3))
            aux1 = valthe*(zr(i1)*xn(3)+zr(i1+1)*yn(3))
            term23 = aux1 + aux2
            if (niv .eq. 2) then
                write(ifm,*)' INOV0/JN0V0/MNOV ',inov0,jnov0,mnov
                write(ifm,*)' ZR MNO P',aux1,aux2
            endif
            if (ltheta) then
                ij = ij+iflum-iflup
                i1 = i1+iavalm-iavalp
                aux4 = -valunt*(zr(ij)*xn(3)+zr(ij+1)*yn(3))
                aux3 = valunt*(zr(i1)*xn(3)+zr(i1+1)*yn(3))
                aux1 = aux1 + aux3
                aux2 = aux2 + aux4
                term23 = term23 + aux3 + aux4
                if (niv .eq. 2) write(ifm,*)' ZR MNO M',aux3,aux4
            endif
            term22 = term22 + jac(3)*term23*term23
            aux1 = (aux1-aux2)*0.5d0
            aux = aux + jac(3)*aux1*aux1
        endif
    else
!
! CAS 3D
        do 100 in = 1, nsomm
!
! NOEUD COURANT
            iino = noe(in,ino,ityp)
            ncher = zi(jad-1+iino)
            if (niv .eq. 2) write(ifm, *)'NOEUD COURANT LOCAL/GLOBAL', iino, ncher
! NOEUD VOISIN CORRESPONDANT
            iinov = indiis(zi(jadv),ncher,1,nbnv)
            if (niv .eq. 2) write(ifm,*)'SON VOISIN LOCAL',iinov
!
            ij = iflup+(iino-1)*ndim
            i1 = iavalp+ndim*((iel-1)*nbnv+iinov-1)
            aux2 = -valthe*(zr(ij)*xn(in)+zr(ij+1)*yn(in)+zr(ij+2)* zn(in))
            aux1 = valthe*(zr(i1)*xn(in)+zr(i1+1)*yn(in)+zr(i1+2)* zn(in))
            term23 = aux1 + aux2
            if (niv .eq. 2) write(ifm,*)' ZR IINO P/IN ',aux1,aux2,in
            if (ltheta) then
                ij = ij+iflum-iflup
                i1 = i1+iavalm-iavalp
                aux4 = -valunt*(zr(ij)*xn(in)+zr(ij+1)*yn(in)+zr(ij+2) * zn(in) )
                aux3 = valunt*(zr(i1)*xn(in)+zr(i1+1)*yn(in)+zr(i1+2)* zn(in))
                aux1 = aux1 + aux3
                aux2 = aux2 + aux4
                term23 = term23 + aux3 + aux4
                if (niv .eq. 2) write(ifm,*)' ZR IINO M    ',aux3,aux4
            endif
            term22 = term22 + term23*term23*jac(in)
            aux1 = (aux1-aux2)*0.5d0
            aux = aux + aux1*aux1*jac(in)
100     continue
!
    endif
!
end subroutine
