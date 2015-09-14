subroutine te0528(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/burftm.h"
#include "asterfort/calcgr.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lcumvi.h"
#include "asterfort/nbsigm.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!
!     BUT: CALCUL DES DEFORMATIONS DUES :
!         AU FLUAGE DE DESSICCATION
!          POUR LE MODELE BETON_UMLV_FP
!         AU FLUAGE PROPRE POUR LES MODELES BETON_UMLV_FP ET GRANGER
!          AUX NOEUDS ET PG
!          ELEMENTS ISOPARAMETRIQUES 3D/D_PLAN/AXIS
!
!
!     IN   OPTION : OPTIONS DE CALCUL
!                   'EPFD_ELGA'
!                   'EPFP_ELGA'
!          NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: jgano, mxcmel, nbsgm, i, ndim, nno, nbsig, nnos, npg, ipoids, ivf
    integer :: idfde, igau, isig, igeom, idef, icompo, nbvari, ivari, jtab(7)
    integer :: iret, imate, itemps
    parameter (mxcmel=162)
    parameter (nbsgm=6)
    real(kind=8) :: epsfl(mxcmel), epstmp(nbsgm)
    real(kind=8) :: valpar, nu(1)
    integer :: icodre(1)
    character(len=8) ::  nompar
    character(len=16) :: nomres, compo1, compo2, valk(2)
    aster_logical :: lflu
! DEB ------------------------------------------------------------------
!
! --- INITIALISATIONS :
!     ---------------
!
! --- RECUPERATION DU COMPORTEMENT  :
!     -------------------------------
    call jevech('PCOMPOR', 'L', icompo)
!
    compo1=zk16(icompo)
    compo2=zk16(icompo+7)
!
!    VERIFICATION DU COMPORTEMENT FLUAGE
    lflu=.false.
    if (option(1:4) .eq. 'EPFD') then
        if ((compo1(1:13).eq.'BETON_UMLV_FP') .or. (compo1(1:15) .eq.'BETON_BURGER_FP')&
            .or. ( compo1(1:7) .eq. 'KIT_DDI' .and. compo2(1:13) .eq. 'BETON_UMLV_FP' )) then
            lflu=.true.
        endif
    else if (option(1:4).eq.'EPFP') then
        if ((compo1(1:13).eq.'BETON_UMLV_FP') .or. (compo1(1:15) .eq.'BETON_BURGER_FP') .or.&
            (compo1(1:10).eq.'GRANGER_FP') .or.&
            (compo1(1:7).eq.'KIT_DDI'.and. compo2(1:10) .eq.'GRANGER_FP') .or.&
            (compo1(1:7).eq.'KIT_DDI'.and. compo2( 1:13).eq.'BETON_UMLV_FP')) lflu= .true.
    endif
!
    if (.not.lflu) then
        valk(1)=option
        valk(2) = compo1
        call utmess('A', 'ELEMENTS4_63', nk=2, valk=valk)
        goto 999
    endif
!
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
!      -----------------------------------------
    nbsig = nbsigm()
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!     ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
!    ------------------------------------------------------------------
    call jevech('PVARIGR', 'L', ivari)
    call tecach('OOO', 'PVARIGR', 'L', iret, nval=7,&
                itab=jtab)
    nbvari = max(jtab(6),1)*jtab(7)
!
    call r8inir(mxcmel, 0.d0, epsfl, 1)
!
!     -----------------------------------------------------------
!      CALCUL DE L'OPTION EPFD
!     -----------------------------------------------------------
!
    if (option(1:4) .eq. 'EPFD') then
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
!
        do 140 igau = 1, npg
!
! POUR BETON_UMLV_FP LE FLUAGE DE DESSICCATION VAUT
!                    [V9 V10 V11 V18 V19 V20]
!
            if ((compo1(1:13).eq.'BETON_UMLV_FP') .or.&
                ( compo1(1:7) .eq. 'KIT_DDI' .and. compo2(1:13) .eq. 'BETON_UMLV_FP' )) then
                call lcumvi('FD', zr(ivari+(igau-1)*nbvari), epstmp)
!
            else if (compo1(1:15).eq.'BETON_BURGER_FP') then
                call burftm('FD', zr(ivari+(igau-1)*nbvari), epstmp)
!
            endif
!
            do 182 i = 1, nbsig
                epsfl(nbsig*(igau-1)+i)=epstmp(i)
182         continue
140     continue
!
!     --------------------------------------------------------
!      CALCUL DE L'OPTION EPFP
!     --------------------------------------------------------
    else if (option(1:4).eq.'EPFP') then
! --- RECUPERATION DU MATERIAU :
!     ------------------------
        call jevech('PMATERC', 'L', imate)
!
! --- RECUPERATION DE L'INSTANT COURANT :
!     ---------------------------------
        call jevech('PTEMPSR', 'L', itemps)
!
        do 180 igau = 1, npg
!
            if ((compo1(1:13).eq.'BETON_UMLV_FP') .or.&
                ( compo1(1:7) .eq. 'KIT_DDI' .and. compo2(1:13) .eq. 'BETON_UMLV_FP' )) then
!      POUR BETON_UMLV LE FLUAGE PROPRE VAUT
!        EPFP11 = (V1+V2) + V3 + V4
!        EPFP22 = (V1+V2) + V5 + V6
!        EPFP33 = (V1+V2) + V7 + V8
!        EPFP12 = V12+V13
!        EPFP13 = V14+V15
!        EPFP14 = V16+V17
!
                call lcumvi('FP', zr(ivari+(igau-1)*nbvari), epstmp)
!
                do 185 i = 1, nbsig
                    epsfl(nbsig*(igau-1)+i)=epstmp(i)
185             continue
!
            else if (compo1(1:15).eq.'BETON_BURGER_FP') then
!      POUR BETON_BURGER LE FLUAGE PROPRE VAUT
!        EPFP11 = (V1+V2) + V3 + V4
!        EPFP22 = (V1+V2) + V5 + V6
!        EPFP33 = (V1+V2) + V7 + V8
!        EPFP12 = V12+V13
!        EPFP13 = V14+V15
!        EPFP14 = V16+V17
!
                call burftm('FP', zr(ivari+(igau-1)*nbvari), epstmp)
!
                do 190 i = 1, nbsig
                    epsfl(nbsig*(igau-1)+i)=epstmp(i)
190             continue
!
!-------------------------------------------------------------------*
                else if ((compo1(1:10).eq.'GRANGER_FP') .or. (compo1(1:7)&
            .eq.'KIT_DDI'.and. compo2(1:10).eq.'GRANGER_FP') )&
            then
                nomres='NU'
                nompar='INST'
                valpar=zr(itemps)
!
                call rcvalb('RIGI', igau, 1, '+', zi(imate),&
                            ' ', 'ELAS', 1, nompar, [valpar],&
                            1, nomres, nu, icodre, 1)
!
                call calcgr(igau, nbsig, nbvari, zr(ivari), nu(1),&
                            epstmp)
                do 187 i = 1, nbsig
                    epsfl(nbsig*(igau-1)+i)=epstmp(i)
187             continue
!
            endif
180     continue
!
    endif
!
! --- RECUPERATION DU VECTEUR EN SORTIE:
!     -------------------------------------------------------------
    call jevech('PDEFOPG', 'E', idef)
!
! --- AFFECTATION DU VECTEUR EN SORTIE
!     ------------------------------------------------------------
! ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
! ---    POINTS D'INTEGRATION :
!        --------------------
    do 160 igau = 1, npg
        do 150 isig = 1, nbsig
            zr(idef+nbsig* (igau-1)+isig-1) = epsfl(nbsig* (igau-1)+ isig)
150     continue
160 end do
!
999 continue
end subroutine
