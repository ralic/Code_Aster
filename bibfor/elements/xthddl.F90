subroutine xthddl(nfh, nddlno, nno, stano, option,&
                  nomte, mat, vect)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sam.cuvilliez at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/indent.h"
#include "asterfort/teattr.h"
    integer, intent(in) :: nfh, nddlno, nno, stano(*)
    character(len=16), intent(in) :: option, nomte
    real(kind=8), optional, intent(inout) :: mat(*)
    real(kind=8), optional, intent(out) :: vect(*)
!
!     BUT: THERMIQUE + ELEMENTS X-FEM LINEAIRES, SUPPRIMER LES DDL "EN
!          TROP" (ATTENTION STOCKAGE SYMETRIQUE POUR LA MATRICE "MAT")
!          ROUTINE EQUIVALENTE EN MECA -> XTEDLL
!
! IN       NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! IN       NDDLNO : NOMBRE DE DDL PAR NOEUD
! IN       NNO    : NOMBRE DE NOEUDS
! IN       STANO  : STATUT DES NOEUDS
! IN       OPTION : OPTION DE CALCUL DU TE
! IN       NOMTE  : NOM DU TYPE ELEMENT
!
! IN/OUT   MAT    : MATRICE DE RIGIDITE OU DE MASSE
! OUT      VECT   : VECTEUR SECOND MEMBRE
!
!-----------------------------------------------------------------------
!---------------- DECLARATION DES VARIABLES LOCALES  -------------------
!
    integer :: ier, istatu, ino, i, j, ielim, in, ddlmax
    integer :: nddl
!      AU PLUS 8*3=24 DDL (MAX ATTEINT POUR L'HEXA8 XHT)
    parameter    (ddlmax=24)
    integer :: posddl(ddlmax)
    character(len=8) :: tyenel
    aster_logical :: lelim, lmat, lvec
    real(kind=8) :: dmax, dmin, codia
!
!-------------------------------------------------------------
!
!
!-------------------------------------------------------------
!   NOMS DES OPTIONS AUTORISEES
!-------------------------------------------------------------
!
    lmat = .false.
    lvec = .false.
!
!   OPTIONS RELATIVES A UNE MATRICE
    if (option .eq. 'RIGI_THER' .or. option .eq. 'RIGI_THER_PARO_F' .or. option .eq.&
        'RIGI_THER_PARO_R' .or. option .eq. 'MASS_THER') then
        lmat = .true.
!   OPTIONS RELATIVES A UN VECTEUR
        elseif (     option .eq. 'CHAR_THER_EVOL'&
            .or. option .eq. 'CHAR_THER_PARO_F'&
            .or. option .eq. 'CHAR_THER_PARO_R' ) then
        lvec = .true.
    else
        ASSERT(.false.)
    endif
!
!-------------------------------------------------------------
!   VERIFICATION DE LA COHERENCE OPTION / ARGUMENTS OPTIONNELS
!-------------------------------------------------------------
!
    if (present(mat) .and. .not.present(vect)) then
        ASSERT(lmat .and. .not.lvec)
    else if (.not.present(mat) .and. present(vect)) then
        ASSERT(.not.lmat .and. lvec)
!   EXACTEMENT UN DES 2 ARGUMENTS mat OU vect EST OBLIGATOIRE
    else
        ASSERT(.false.)
    endif
!
!-------------------------------------------------------------
!
! --- TYPE D'ENRICHISSEMENT DE L'ELEMENT ET TYPE D'ELIMINATION
!
    call teattr('S', 'XFEM', tyenel, ier, typel=nomte)
    if (tyenel(1:2) .eq. 'XH') ielim=1
    if (tyenel(1:2) .eq. 'XT') ielim=2
    if (tyenel(1:3) .eq. 'XHT') ielim=3
!
!     REMPLISSAGE DU VECTEUR POS : POSITION DES DDLS A SUPPRIMER
!
    nddl = nddlno*nno
    ASSERT(nddl.le.ddlmax)
    do 99 ino = 1, ddlmax
        posddl(ino)=0
 99 end do
!
!     VRAI SI ON ELIMINE LES DDLS D'AU MOINS UN NOEUD
    lelim=.false.
!
    do 100 ino = 1, nno
!
        call indent(ino, nddlno, 0, nno, in)
!
        if (ielim .eq. 1) then
!         1) CAS DES MAILLES 'ROND'
!         -------------------------
!         STATUT DES NOEUDS ENRICHIS
            istatu = stano(ino)
            ASSERT(istatu.le.1)
            if (istatu .eq. 0) then
!           ON SUPPRIME LES DDL H
                posddl(in+1+1)=1
                lelim=.true.
            endif
!
        else if (ielim.eq.2) then
!         2) CAS DES MAILLES 'CARRÉ'
!         --------------------------
!         STATUT DES NOEUDS ENRICHIS
            istatu = stano(ino)
            ASSERT(istatu.le.2 .and. istatu.ne.1)
            if (istatu .eq. 0) then
!           ON SUPPRIME LES DDL E
                posddl(in+1+nfh+1)=1
                lelim=.true.
            endif
!
        else if (ielim.eq.3) then
!         3) CAS DES MAILLES 'ROND-CARRÉ'
!         ------------------------------
!         STATUT DES NOEUDS ENRICHIS
            istatu = stano(ino)
            ASSERT(istatu.le.3)
            if (istatu .eq. 2) then
!           ON SUPPRIME LES DDL H
                posddl(in+1+1)=1
                lelim=.true.
            else if (istatu.eq.1) then
!           ON SUPPRIME LES DDL E
                posddl(in+1+nfh+1)=1
                lelim=.true.
            else if (istatu.eq.0) then
!           ON SUPPRIME LES DDL H ET E
                posddl(in+1+1)=1
                posddl(in+1+nfh+1)=1
                lelim=.true.
            endif
!
        endif
!
100 end do
!
    if (lelim) then
!
!     POUR LES OPTIONS CONCERNANT DES MATRICES :
!       CALCUL DU COEFFICIENT DIAGONAL POUR
!       L'ELIMINATION DES DDLS HEAVISIDE
        if (lmat) then
            dmin=r8maem()
            dmax=-r8maem()
            do 110 i = 1, nddl
                codia=mat((i-1)*i/2+i)
                if (codia .gt. dmax) then
                    dmax=codia
                else if (codia.lt.dmin) then
                    dmin=codia
                endif
110         continue
            codia=(dmax+dmin)/2.0d0
            if (codia .eq. 0) codia = 1
        endif
!
        do 200 i = 1, nddl
            if (posddl(i) .eq. 0) goto 200
!         POUR LES OPTIONS CONCERNANT DES MATRICES :
!           MISE A ZERO DES TERMES HORS DIAGONAUX (I,J)
!           ET MISE A UN DES TERMES DIAGONAUX (I,I)
!           (ATTENTION AU STOCKAGE SYMETRIQUE)
            if (lmat) then
                do 210 j = 1, nddl
                    if (j .lt. i) mat((i-1)*i/2+j) = 0.d0
                    if (j .eq. i) mat((i-1)*i/2+j) = codia
                    if (j .gt. i) mat((j-1)*j/2+i) = 0.d0
210             continue
            endif
!         POUR LES OPTIONS CONCERNANT DES VECTEURS :
!           MISE A ZERO DES TERMES I
            if (lvec) vect(i) = 0.d0
200     continue
!
    endif
!
end subroutine
