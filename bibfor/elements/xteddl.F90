subroutine xteddl(ndim, nfh, nfe, ddls, nddl,&
                  nno, nnos, stano, lcontx, matsym,&
                  option, nomte, mat, vect, ddlm,&
                  nfiss, jfisno)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'jeveux.h'
    include 'asterc/r8maem.h'
    include 'asterfort/assert.h'
    include 'asterfort/indent.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/teattr.h'
    integer :: ndim, nfh, nfe, ddls, nddl, nno, nnos, stano(*)
    logical :: matsym, lcontx
    character(len=16) :: option, nomte
    real(kind=8) :: mat(*), vect(*)
    integer :: ddlm, nfiss, jfisno
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
    integer :: posddl(ddlmax)
    character(len=8) :: tyenel
    logical :: lelim, lmultc
    real(kind=8) :: dmax, dmin, codia
!
!-------------------------------------------------------------
!
    call jemarq()
!
! --- CONECTIVITÉ DES FISSURE ET DES DDL HEAVISIDES
!
    if (nfiss .eq. 1) then
        do 300 ino = 1, nno
            fisno(ino,1) = 1
300      continue
    else
        do 310 ifh = 1, nfh
            do 320 ino = 1, nno
                fisno(ino,ifh) = zi(jfisno-1+(ino-1)*nfh+ifh)
320          continue
310      continue
    endif
!     TYPE D'ENRICHISSEMENT DE L'ELEMENT ET TYPE D'ELIMINATION
    call teattr(nomte, 'S', 'XFEM', tyenel, ier)
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
    call assert(nddl.le.ddlmax)
    do 99 ino = 1, ddlmax
        posddl(ino)=0
99  end do
!
!     VRAI SI ON ELIMINE LES DDLS D'AU MOINS UN NOEUD
    lelim=.false.
!
    do 100 ino = 1, nno
        call indent(ino, ddls, ddlm, nnos, in)
!       ENRICHISSEMENT DU NOEUD
        if (ielim .eq. 1) then
!
!         1) CAS DES MAILLES 'ROND'
!         -------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            do 330 ifh = 1, nfh
                istatu = stano((ino-1)*nfiss+fisno(ino,ifh))
                call assert(istatu.le.1)
                if (istatu .eq. 0) then
!           ON SUPPRIME LES DDL H
                    do 10 k = 1, ndim
                        posddl(in+ndim*ifh+k)=1
!           ON SUPPRIME LES DDL C SI MULTI-HEAVISIDE AVEC CONTACT
                        if (lmultc) posddl(in+ndim*(nfh+ifh)+k)=1
10                  continue
                    lelim=.true.
                endif
330          continue
        else if (ielim.eq.2) then
!
!         2) CAS DES MAILLES 'CARRÉ'
!         --------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            istatu = stano(ino)
            call assert(istatu.le.2 .and. istatu.ne.1)
            if (istatu .eq. 2) then
!           ON NE SUPPRIME AUCUN DDL
            else if (istatu.eq.0) then
!           ON SUPPRIME LES DDL E
                do 20 k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
20              continue
                lelim=.true.
            endif
!
        else if (ielim.eq.3) then
!
!         3) CAS DES MAILLES 'ROND-CARRÉ'
!         ------------------------------
!
!         PB DE STATUT DES NOEUDS ENRICHIS
            istatu = stano(ino)
            call assert(istatu.le.3)
            if (istatu .eq. 3) then
!           ON NE SUPPRIME AUCUN DDL
            else if (istatu.eq.2) then
!           ON SUPPRIME LES DDL H
                do 30 k = 1, ndim
                    posddl(in+ndim+k)=1
30              continue
                lelim=.true.
            else if (istatu.eq.1) then
!           ON SUPPRIME LES DDL E
                do 40 k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
40              continue
                lelim=.true.
            else if (istatu.eq.0) then
!           ON SUPPRIME LES DDLS H ET E
                do 50 k = 1, ndim
                    posddl(in+ndim+k)=1
50              continue
                do 60 k = 1, nfe*ndim
                    posddl(in+ndim*(1+nfh)+k)=1
60              continue
                lelim=.true.
            endif
!
        else if (ielim.eq.4) then
!         4) CAS DU CONTACT
!         ------------------------------
            if (ino .le. nnos) then
!
                do 80 ifh = 1, max(1, nfh)
                    istatu = stano((ino-1)*max(1,nfh)+ifh)
                    if (istatu .eq. 0) then
!             ON SUPPRIME LES DDLS LAGS_C, LAGS_F1 ET LAGS_F2
                        do 70 k = 1, ndim
                            posddl(in+ndim*(nfh+nfe+ifh)+k)=1
70                      continue
                        lelim=.true.
                    endif
80              continue
            endif
        endif
!
100  end do
!
    if (lelim) then
!
!     POUR LES OPTIONS CONCERNANT DES MATRICES :
!        CALCUL DU COEFFICIENT DIAGONAL POUR
!        L'ELIMINATION DES DDLS HEAVISIDE
        if (option(1:10) .eq. 'RIGI_MECA_' .or. option .eq. 'RIGI_MECA' .or. option .eq.&
            'FULL_MECA' .or. option .eq. 'RIGI_CONT' .or. option .eq. 'RIGI_FROT' .or.&
            option .eq. 'MASS_MECA') then
            dmin=r8maem()
            dmax=-r8maem()
            do 110 i = 1, nddl
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
110          continue
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
        do 200 i = 1, nddl
            if (posddl(i) .eq. 0) goto 200
            if (option(1:10) .eq. 'RIGI_MECA_' .or. option .eq. 'RIGI_MECA' .or. option&
                .eq. 'FULL_MECA' .or. option .eq. 'RIGI_CONT' .or. option .eq. 'RIGI_FROT'&
                .or. option .eq. 'MASS_MECA') then
                do 210 j = 1, nddl
                    if (matsym) then
                        if (j .lt. i) mat((i-1)*i/2+j) = 0.d0
                        if (j .eq. i) mat((i-1)*i/2+j) = codia
                        if (j .gt. i) mat((j-1)*j/2+i) = 0.d0
                    else
                        if (j .ne. i) mat((i-1)*nddl+j) = 0.d0
                        if (j .ne. i) mat((j-1)*nddl+i) = 0.d0
                        if (j .eq. i) mat((i-1)*nddl+j) = codia
                    endif
210              continue
            endif
            if (option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA' .or. option .eq.&
                'FORC_NODA' .or. option .eq. 'CHAR_MECA_PRES_R' .or. option .eq.&
                'CHAR_MECA_PRES_F' .or. option .eq. 'CHAR_MECA_FR2D3D' .or. option .eq.&
                'CHAR_MECA_FR1D2D' .or. option .eq. 'CHAR_MECA_FF2D3D' .or. option .eq.&
                'CHAR_MECA_FF1D2D' .or. option .eq. 'CHAR_MECA_CONT' .or. option .eq.&
                'CHAR_MECA_FROT' .or. option .eq. 'CHAR_MECA_FR3D3D' .or. option .eq.&
                'CHAR_MECA_FR2D2D' .or. option .eq. 'CHAR_MECA_FF3D3D' .or. option .eq.&
                'CHAR_MECA_FF2D2D' .or. option .eq. 'CHAR_MECA_PESA_R' .or. option .eq.&
                'CHAR_MECA_ROTA_R' .or. option .eq. 'CHAR_MECA_TEMP_R') vect(i) = 0.d0
200      continue
!
    endif
!
    call jedema()
end subroutine
