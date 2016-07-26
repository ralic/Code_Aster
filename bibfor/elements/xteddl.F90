subroutine xteddl(ndim, nfh, nfe, ddls, nddl,&
                  nno, nnos, stano, lcontx, matsym,&
                  option, nomte, ddlm, nfiss, jfisno,&
                  mat, vect)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/indent.h"
#include "asterfort/teattr.h"
#include "asterfort/is_enr_line.h"
    integer, intent(in) :: ndim, nfh, nfe, ddls, nddl, nno, nnos, stano(*)
    aster_logical, intent(in) :: matsym, lcontx
    character(len=16), intent(in) :: option, nomte
    integer, intent(in) :: ddlm, nfiss, jfisno
    real(kind=8), optional, intent(inout) :: mat(*)
    real(kind=8), optional, intent(out) :: vect(*)
!     BUT: SUPPRIMER LES DDLS "EN TROP" (VOIR BOOK III 09/06/04
!                                         ET  BOOK IV  30/07/07)
!
! IN   NDIM   : DIMENSION DE L'ESPACE
! OUT  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE
! OUT  NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN   DDLS   : NOMBRE DE DDL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
! IN   NDDL   : NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
! IN   NNO    : NOMBRE DE NOEUDS DE L'ELEMENT PORTANT DES DDLS DE DEPL
! IN   NNOS   : NOMBRE DE NOEUDS SOMMENT DE L'ELEMENT
! IN   STANO  : STATUT DES NOEUDS
! IN   LCONTX : ON S'OCCUPE DES DDLS DE CONTACT
! IN   MATSYM : STOCKAGE DE LA MATRICE SYMÉTRIQUE ?
! IN   OPTION : OPTION DE CALCUL DU TE
! IN   NOMTE  : NOM DU TYPE ELEMENT
! IN   NFISS  : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN   JFISNO : POINTEUR DE CONNECTIVITÉ FISSURE/HEAVISIDE
!
! IN/OUT :   MAT   : MATRICE DE RIGIDITÉ
! IN/OUT :   VECT  : VECTEUR SECOND MEMBRE
!
! IN   DDLM   : NOMBRE DE DDL (DEPL+CONTACT) A CHAQUE NOEUD MILIEU
!
!-----------------------------------------------------------------------
!---------------- DECLARATION DES VARIABLES LOCALES  -------------------
!
    integer :: ier, istatu, ino, k, i, j, ielim, in, ddlmax
    integer :: ifh, fisno(nno, nfiss)
    parameter    (ddlmax=1053)
    integer :: posddl(ddlmax), ddlc, nlag
    character(len=8) :: tyenel
    aster_logical :: lelim, lmultc, lmat, lvec, lctlin
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
    lctlin = is_enr_line()
!
!   OPTIONS RELATIVES A UNE MATRICE
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_GE' .or. option .eq.&
        'RIGI_MECA_TANG' .or. option .eq. 'RIGI_MECA' .or. option .eq. 'RIGI_CONT' .or. option&
        .eq. 'RIGI_FROT' .or. option .eq. 'MASS_MECA'&
        .or. option .eq. 'RIGI_CONT_M') lmat = .true.
!
!   OPTIONS RELATIVES A UN VECTEUR
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA' .or. option .eq. 'FORC_NODA' .or.&
        option .eq. 'CHAR_MECA_PRES_R' .or. option .eq. 'CHAR_MECA_PRES_F' .or. option .eq.&
        'CHAR_MECA_FR2D3D' .or. option .eq. 'CHAR_MECA_FR1D2D' .or. option .eq.&
        'CHAR_MECA_FF2D3D' .or. option .eq. 'CHAR_MECA_FF1D2D' .or. option .eq.&
        'CHAR_MECA_CONT' .or. option .eq. 'CHAR_MECA_FROT' .or. option .eq. 'CHAR_MECA_FR3D3D'&
        .or. option .eq. 'CHAR_MECA_FR2D2D' .or. option .eq. 'CHAR_MECA_FF3D3D' .or. option&
        .eq. 'CHAR_MECA_FF2D2D' .or. option .eq. 'CHAR_MECA_PESA_R' .or. option .eq.&
        'CHAR_MECA_ROTA_R' .or. option .eq. 'CHAR_MECA_TEMP_R' .or. option .eq.&
        'CHAR_MECA_EFON_R' .or. option .eq. 'CHAR_MECA_EFON_F'&
        .or. option .eq. 'CHAR_MECA_CONT_M') lvec = .true.
!
    ASSERT(lmat .or. lvec)
!
!-------------------------------------------------------------
!   VERIFICATION DE LA COHERENCE OPTION / ARGUMENTS OPTIONNELS
!-------------------------------------------------------------
!
    if (present(mat) .and. present(vect)) then
        ASSERT(lmat .and. lvec)
    else if (present(mat) .and. .not.present(vect)) then
        ASSERT(lmat .and. .not.lvec)
    else if (.not.present(mat) .and. present(vect)) then
        ASSERT(.not.lmat .and. lvec)
!   AU MOINS UN DES 2 ARGUMENTS mat OU vect EST OBLIGATOIRE
    else
        ASSERT(.false.)
    endif
!
!-------------------------------------------------------------
!
! --- CONECTIVITÉ DES FISSURE ET DES DDL HEAVISIDES
!
    if (nfiss .eq. 1) then
        do ino = 1, nno
            fisno(ino,1) = 1
        enddo
    else
        do ifh = 1, nfh
            do ino = 1, nno
                fisno(ino,ifh) = zi(jfisno-1+(ino-1)*nfh+ifh)
            enddo
        enddo
    endif
!     TYPE D'ENRICHISSEMENT DE L'ELEMENT ET TYPE D'ELIMINATION
    call teattr('S', 'XFEM', tyenel, ier, typel=nomte)
    if (tyenel(1:2) .eq. 'XH') ielim=1
    if (tyenel(1:2) .eq. 'XT') ielim=2
    if (tyenel(1:3) .eq. 'XHT') ielim=3
    if (lcontx) ielim=4
!     APPROCHE (HIX, HIY, HIZ) <-> (LAGI_C,LAGI_F1,LAGI_F2) POUR MULTI-H
    if (nfiss .gt. 1 .and. tyenel(4:4) .eq. 'C') then
        lmultc = .true.
    else
        lmultc = .false.
    endif
!
!     REMPLISSAGE DU VECTEUR POS : POSITION DES DDLS A SUPPRIMER
    ASSERT(nddl.le.ddlmax)
    do ino = 1, ddlmax
        posddl(ino)=0
    end do
!
!     VRAI SI ON ELIMINE LES DDLS D'AU MOINS UN NOEUD
    lelim=.false.
!
    do ino = 1, nno
        call indent(ino, ddls, ddlm, nnos, in)
!       ENRICHISSEMENT DU NOEUD
        if (ielim .eq. 1) then
!
!         1) CAS DES MAILLES 'ROND'
!         -------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            do ifh = 1, nfh
                istatu = stano((ino-1)*nfiss+fisno(ino,ifh))
                ASSERT(istatu.le.1)
                if (istatu .eq. 0) then
!           ON SUPPRIME LES DDL H
                    do k = 1, ndim
                        posddl(in+ndim*ifh+k)=1
!           ON SUPPRIME LES DDL C SI MULTI-HEAVISIDE AVEC CONTACT
                        if (lmultc) posddl(in+ndim*(nfh+ifh)+k)=1
                    enddo
                    lelim=.true.
                endif
            enddo
        else if (ielim.eq.2) then
!
!         2) CAS DES MAILLES 'CARRÉ'
!         --------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            istatu = abs(stano(ino))
            ASSERT(istatu.le.2 .and. istatu.ne.1)
            if (istatu .eq. 2) then
!           ON SUPPRIME LES DDLS VECTORIELS DES NOEUDS MILIEUX
                if (ino.gt.nnos.and.lctlin) then
                do k = 1, ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                endif
                lelim=.true.
            elseif (istatu.eq.0) then
!           ON SUPPRIME LES DDL E
                do k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                lelim=.true.
            endif
!
        else if (ielim.eq.3) then
!
!         3) CAS DES MAILLES 'ROND-CARRÉ'
!         ------------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            istatu = abs(stano(ino))
            ASSERT(istatu.le.3)
            if (istatu .eq. 3) then
!           ON SUPPRIME LES DDLS VECTORIELS DES NOEUDS MILIEUX
                if (ino.gt.nnos.and.lctlin) then
                do k = 1, ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                endif
                lelim=.true.
            elseif (istatu.eq.2) then
!           ON SUPPRIME LES DDL H
                do k = 1, ndim
                    posddl(in+ndim+k)=1
                enddo
!           ON SUPPRIME LES DDLS VECTORIELS DES NOEUDS MILIEUX
                if (ino.gt.nnos.and.lctlin) then
                do k = 1, ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                endif
                lelim=.true.
            else if (istatu.eq.1) then
!           ON SUPPRIME LES DDL E
                do k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                lelim=.true.
            else if (istatu.eq.0) then
!           ON SUPPRIME LES DDLS H ET E
                do k = 1, ndim
                    posddl(in+ndim+k)=1
                enddo
                do k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
                enddo
                lelim=.true.
            endif
!
        else if (ielim.eq.4) then
!         4) CAS DU CONTACT
!         ------------------------------
            ddlc = ddls-ndim*(1+nfh+nfe)
            nlag = ddlc/ndim
            if(lmultc) nlag=1
            if (ino .le. nnos) then
                do ifh = 1, nlag*max(1, nfh)
                    if(lmultc) istatu = stano((ino-1)*max(1,nfh)+ifh)
                    if(.not.lmultc) istatu = stano((ino-1)*max(1,nfh)+1)
                    if (istatu .eq. 0) then
!             ON SUPPRIME LES DDLS LAGS_C, LAGS_F1 ET LAGS_F2
                        do k = 1, ndim
                            posddl(in+ndim*(nfh+nfe+ifh)+k)=1
                        enddo
                        lelim=.true.
                    endif
                enddo
            endif
        endif
!
    end do
!
    if (lelim) then
!
!     POUR LES OPTIONS CONCERNANT DES MATRICES :
!        CALCUL DU COEFFICIENT DIAGONAL POUR
!        L'ELIMINATION DES DDLS HEAVISIDE
        if (lmat) then
            dmin=r8maem()
            dmax=-r8maem()
            do i = 1, nddl
                if (matsym) then
                    codia=mat((i-1)*i/2+i)
                else
                    codia=mat((i-1)*nddl+i)
                endif
                if (codia .gt. dmax) then
                    dmax=codia
                else if (codia.lt.dmin) then
                    dmin=codia
                endif
            enddo
            codia=(dmax+dmin)/2.0d0
            if (codia .eq. 0) codia = 1
        endif
!
!     POUR LES OPTIONS CONCERNANT DES MATRICES :
!        MISE A ZERO DES TERMES HORS DIAGONAUX (I,J)
!        ET MISE A UN DES TERMES DIAGONAUX (I,I)
!        (ATTENTION AU STOCKAGE SYMETRIQUE)
!     POUR LES OPTIONS CONCERNANT DES VECTEURS :
!        MISE A ZERO DES TERMES I
!
        do i = 1, nddl
            if (posddl(i) .eq. 0) goto 199
            if (lmat) then
                do j = 1, nddl
                    if (matsym) then
                        if (j .lt. i) mat((i-1)*i/2+j) = 0.d0
                        if (j .eq. i) mat((i-1)*i/2+j) = codia
                        if (j .gt. i) mat((j-1)*j/2+i) = 0.d0
                    else
                        if (j .ne. i) mat((i-1)*nddl+j) = 0.d0
                        if (j .ne. i) mat((j-1)*nddl+i) = 0.d0
                        if (j .eq. i) mat((i-1)*nddl+j) = codia
                    endif
                enddo
            endif
            if (lvec) vect(i) = 0.d0
199         continue
        enddo
!
    endif
!
end subroutine
