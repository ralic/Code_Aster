subroutine xhmddl(ndim, nfh, ddls, nddl, nno, nnos,&
                  stano, matsym, option, nomte, mat,&
                  vect, ddlm, nfiss, jfisno, lcontx,&
                  contac)
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
! person_in_charge: daniele.colombo at ifpen.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/hmdeca.h"
#include "asterc/r8maem.h"
#include "asterfort/teattr.h"
#include "jeveux.h"
    aster_logical :: matsym, lcontx
    integer :: ndim, ddls, nddl, nno, nnos, stano(*), ddlm, nfh
    integer :: nfiss, jfisno, dec, contac
    character(len=16) :: option, nomte
    real(kind=8) :: mat(*), vect(*)
!
!     BUT: SUPPRIMER LES DDLS "EN TROP"
!
! IN   NDIM   : DIMENSION DE L'ESPACE
! IN   DDLS   : NOMBRE DE DDL A CHAQUE NOEUD SOMMET
! IN   DDLM   : NOMBRE DE DDL A CHAQUE NOEUD MILIEU
! IN   NDDL   : NOMBRE DE DDL TOTAL DE L'ÉLÉMENT
! IN   NNO    : NOMBRE DE NOEUDS DE L'ELEMENT PORTANT DES DDLS DE DEPL
! IN   NNOS   : NOMBRE DE NOEUDS SOMMENT DE L'ELEMENT
! IN   STANO  : STATUT DES NOEUDS
! IN   OPTION : OPTION DE CALCUL DU TE
! IN   NOMTE  : NOM DU TYPE ELEMENT
! IN   NFISS  : NOMBRE DE FISSURES "VUES" PAR L'ELEMENT
! IN   JFISNO : POINTEUR DE CONNECTIVITE FISSURE/HEAVISIDE
! IN   LCONTX : INDIQUE SI IL Y A DU CONTACT OU PAS 
! IN   CONTAC : TYPE DE CONTACT
!
! IN/OUT :   MAT   : MATRICE DE RIGIDITÉ
! IN/OUT :   VECT  : VECTEUR SECOND MEMBRE
!
!
!-----------------------------------------------------------------------
!---------------- DECLARATION DES VARIABLES LOCALES  -------------------
!
    aster_logical :: lelim
    integer :: ier, istatu, ino, k, i, j, ielim, in, ddlmax
    parameter    (ddlmax=52*20)
    integer :: posddl(ddlmax), ifh, fisno(nno, nfiss)
    real(kind=8) :: dmax, dmin, codia
    character(len=8) :: tyenel
!
!-------------------------------------------------------------
!
!
! --- CONNECTIVITE DES FISSURE ET DES DDL HEAVISIDES
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
!
!     TYPE D'ENRICHISSEMENT DE L'ELEMENT ET TYPE D'ELIMINATION
!
    call teattr('S', 'XFEM', tyenel, ier, typel=nomte)
    if (tyenel(1:2) .eq. 'XH') ielim=1
    if (lcontx) ielim=2
!
!     REMPLISSAGE DU VECTEUR POS : POSITION DES DDLS A SUPPRIMER
!
    ASSERT(nddl.le.ddlmax)
    do ino = 1, ddlmax
        posddl(ino)=0
    end do
!
!     VRAI SI ON ELIMINE LES DDLS D'AU MOINS UN NOEUD
    lelim=.false.
!
    do ino = 1, nno
        call hmdeca(ino, ddls, ddlm, nnos, in, dec)
!
        if (ielim .eq. 1) then
            do ifh = 1, nfh
               istatu = stano((ino-1)*nfiss+fisno(ino,ifh))
               ASSERT(istatu.le.1)
               if (istatu .eq. 0) then
!              ON SUPPRIME LES DDL H MECA ET HYDRO
                   do k = 1, ndim+dec
                        posddl(in+(ndim+dec)*ifh+k)=1
                   end do
                   lelim=.true.
               endif
            end do
        else if (ielim .eq. 2) then 
!           ON SUPPRIME LES DDLS PRE_FLU, LAG_FLI, LAG_FLS, LAG1_HM ET 
!           LAG2_HM AUX NOEUDS SOMMETS
            do ifh = 1, nfh
               if (ino.le.nnos) then 
                   istatu = stano((ino-1)*max(1,nfh)+ifh)
                   if (istatu.eq.0) then 
                       if (contac.eq.3) then
                          do k = 1, 3+ndim
                              posddl(in+(ndim+dec)*(nfh+1)+(ifh-1)*(ndim+3)+k)=1
                          end do
                       else if (contac.eq.2) then
                          do k = 1, 3+3*ndim
                              posddl(in+(ndim+dec)*(nfh+1)+(ifh-1)*(3*ndim+3)+k)=1
                          end do
                       endif
                       lelim=.true.
                   endif
                endif
            end do
        endif
    end do
!
    if (lelim) then
!
!     POUR LES OPTIONS CONCERNANT DES MATRICES :
!        CALCUL DU COEFFICIENT DIAGONAL POUR
!        L'ELIMINATION DES DDLS HEAVISIDE
        if (option(1:10) .eq. 'RIGI_MECA_'& 
            .or. option .eq. 'RIGI_MECA'& 
            .or. option .eq. 'FULL_MECA'& 
            .or. option(1:9) .eq. 'RIGI_CONT') then
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
110         continue
            codia=(dmax+dmin)/2.0d0
            if (codia .eq. 0.d0) codia = 1
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
                .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RIGI_CONT') then
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
210             continue
            endif
            if (option .eq. 'RAPH_MECA' .or. option .eq. 'FULL_MECA' .or. option .eq.&
                'FORC_NODA' .or. option .eq. 'CHAR_MECA_PRES_R' .or. option .eq.&
                'CHAR_MECA_PRES_F' .or. option .eq. 'CHAR_MECA_FLUX_R' .or. option .eq.&
                'CHAR_MECA_FLUX_F' .or. option(1:14) .eq. 'CHAR_MECA_CONT' .or. option .eq.&
                'CHAR_MECA_PESA_R') vect(i) = 0.d0

200     continue
    endif
!
end subroutine
