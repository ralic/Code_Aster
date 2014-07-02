subroutine te0334(option, nomte)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcgr.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/epsvmc.h"
#include "asterfort/granvi.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/nbsigm.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     BUT: CALCUL DES DEFORMATIONS PLASTIQUES AUX NOEUDS ET PG ET DES
!          DEFORMATIONS DE FLUAGE DE GRANGER
!          ELEMENTS ISOPARAMETRIQUES 2D
!
!     IN   OPTION : OPTIONS DE CALCUL
!                   'EPSP_ELGA'
!          NOMTE  : NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: mxcmel, nbres, nbsgm, i, ndim, nno, nnos, nbsig, idsig, icompo
    integer :: npg, ipoids, ivf, idfde, igau, isig, igeom, idepl, idefp, itemps
    integer :: imate, nbvari, ivari, nvif, ibid, jtab(7), iret, jgano
    parameter (mxcmel=54)
    parameter (nbres=3)
    parameter (nbsgm=4)
    real(kind=8) :: valres(nbres), epsm(mxcmel), epspla(mxcmel)
    real(kind=8) :: sigma(nbsgm), valpar, c1, c2, trsig
    real(kind=8) :: epsflf(nbsgm)
    real(kind=8) :: repere(7), nharm, e, nu, zero, un
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar, mod3d
    character(len=16) :: optio2, phenom, cmp1, cmp2
    character(len=16) :: compor
    aster_logical :: lflu
! DEB ------------------------------------------------------------------
!
! --- CARACTERISTIQUES DU TYPE D'ELEMENT :
! --- GEOMETRIE ET INTEGRATION
!     ------------------------
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
!
! --- INITIALISATIONS :
!     ---------------
    zero = 0.0d0
    un = 1.0d0
    nharm = zero
    mod3d = '3D'
!
! --- NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT :
!     -----------------------------------------
    nbsig = nbsigm()
!
! --- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!     ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! --- RECUPERATION DU MATERIAU :
!     ------------------------
    call jevech('PMATERC', 'L', imate)
!
! --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
!     ------------------------------------------------------------
!     ON FOURNIT UN REPRE BIDON A EPSVMC CAR DE TOUTE FACON ON NE
!     TRAITE PAS LE CAS ORTHOTROPE
!
    do 200 i = 1, 7
        repere(i)=0.d0
200 end do
!
! --- RECUPERATION DE L'INSTANT COURANT :
!     ---------------------------------
    call jevech('PTEMPSR', 'L', itemps)
!
!
! ---    RECUPERATION DU CHAMP DE DEPLACEMENTS AUX NOEUDS  :
!        ------------------------------------------------
    call jevech('PDEPLAR', 'L', idepl)
!
! ---    RECUPERATION DU CHAMP DE CONTRAINTES AUX POINTS D'INTEGRATION :
!        -------------------------------------------------------------
    call jevech('PCONTRR', 'L', idsig)
!
! ---    ON VERIFIE QUE LE MATERIAU EST ISOTROPE
! ---    (POUR L'INSTANT PAS D'ORTHOTROPIE NI D'ISOTROPIE TRANSVERSE
! ---    EN PLASTICITE) :
!        --------------
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS_ORTH' .or. phenom .eq. 'ELAS_ISTR') then
        call utmess('F', 'ELEMENTS3_75', sk=phenom(1:12))
    endif
!
! ---    RECUPERATION DU COMPORTEMENT DANS LE CAS DES CONTRAINTES
! ---    PLANES :
!        ---------------------------------------------------------
    if (lteatt('C_PLAN','OUI')) then
        call tecach('ONN', 'PCOMPOR', 'L', iret, iad=icompo)
        if (icompo .ne. 0) then
            compor = zk16(icompo)
            if (compor .ne. 'VMIS_ISOT_LINE' .and. compor(1:4) .ne. 'ELAS' .and. compor&
                .ne. 'VMIS_ISOT_TRAC') then
                call utmess('A', 'ELEMENTS3_77', sk=compor)
            endif
        endif
    endif
!
! ---    CALCUL DES DEFORMATIONS HORS THERMIQUES CORRESPONDANTES AU
! ---    CHAMP DE DEPLACEMENT I.E. EPSM = EPST - EPSTH - EPSRET
! ---    OU EPST  SONT LES DEFORMATIONS TOTALES
! ---       EPST = B.U
! ---    ET EPSTH SONT LES DEFORMATIONS THERMIQUES
! ---       EPSTH = ALPHA*(T-TREF) :
! ---    ET EPSRET SONT LES DEFORMATIONS LIEES AU RETRAIT
! ---       DE DESSICCATION ET  D HYDRATION
! ---       EPSRET = - B_ENDO * HYDR - K_DESSIC *(SREF-S)
!          ----------------------
!
    optio2 = 'EPME_ELGA'
    call epsvmc('RIGI', nno, ndim, nbsig, npg,&
                ipoids, ivf, idfde, zr(igeom), zr(idepl),&
                zr(itemps), zi(imate), repere, nharm, optio2,&
                epsm)
!
! --- RECUPERATION DU COMPORTEMENT  :
!     -------------------------------
    call jevech('PCOMPOR', 'L', icompo)
!
! --- RECUPERATION DES VARIABLES INTERNES AUX PT D'INTEGRATION COURANT :
!     -----------------------------------------------------------------
    call jevech('PVARIGR', 'L', ivari)
    call tecach('OON', 'PVARIGR', 'L', iret, nval=7,&
                itab=jtab)
    nbvari = max(jtab(6),1)*jtab(7)
!
! --- VERIFICATION DU COMPORTEMENT FLUAGE :
!     -------------------------------------
    cmp1 = zk16(icompo)
    cmp2 = zk16(icompo+7)
    if (cmp1(1:10) .ne. 'GRANGER_FP' .and.&
        (cmp1(1:7).ne.'KIT_DDI'.or.cmp2(1:10).ne.'GRANGER_FP')) then
        lflu = .false.
        do 40 i = 1, mxcmel
            epspla(i) = zero
 40     continue
        do 50 i = 1, nbsig
            epsflf(i) = zero
 50     continue
    else
        call granvi(mod3d, ibid, ibid, nvif)
        lflu = .true.
    endif
!
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     -----------------------------------
    do 120 igau = 1, npg
!
!
!
! ---    RECUPERATION DES CARACTERISTIQUES DU MATERIAU :
!        ---------------------------------------------
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
!
        nompar = 'INST'
        valpar = zr(itemps)
!
        call rcvalb('RIGI', igau, 1, '+', zi(imate),&
                    ' ', 'ELAS', 1, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
!
        call rcvalb('RIGI', igau, 1, '+', zi(imate),&
                    ' ', 'ELAS', 1, nompar, [valpar],&
                    1, nomres(3), valres(3), icodre(3), 0)
!
        e = valres(1)
        nu = valres(2)
        if (icodre(3) .ne. 0) then
            valres(3) = zero
        endif
!
! ---    TENSEUR DE DEFORMATION DE FLUAGE AU PT D'INTEGRATION COURANT :
!        --------------------------------------------------------------
        if (lflu) then
!
            call calcgr(igau, nbsig, nbvari, zr(ivari), nu,&
                        epsflf)
!
        endif
!
! ---       TENSEUR DES CONTRAINTES AU POINT D'INTEGRATION COURANT :
!           ------------------------------------------------------
        do 100 i = 1, nbsig
            sigma(i) = zr(idsig+ (igau-1)*nbsig+i-1)
100     continue
!
        if (lteatt('C_PLAN','OUI')) then
            trsig = sigma(1) + sigma(2)
        else
            trsig = sigma(1) + sigma(2) + sigma(3)
        endif
!
        c1 = (un+nu)/e
        c2 = nu/e
!
! ---       TENSEUR DES DEFORMATIONS PLASTIQUES AU POINT
! ---       D'INTEGRATION COURANT
! ---       I.E. EPSPLA = EPS_TOT - EPS_THERM - EPS_ELAS - EPS_ANELAS :
! ---                             - EPS_FLUAGE :
!           ---------------------------------------------------------
        epspla(nbsig* (igau-1)+1) = epsm(nbsig* (igau-1)+1) - (c1* sigma(1)-c2*trsig) - epsflf(1)
        epspla(nbsig* (igau-1)+2) = epsm(nbsig* (igau-1)+2) - (c1* sigma(2)-c2*trsig) - epsflf(2)
        if (lteatt('C_PLAN','OUI')) then
            epspla(nbsig* (igau-1)+3) = - (epspla( nbsig* (igau-1)+1)+ epspla(nbsig* (igau-1)+2 )&
                                        )
        else
            epspla(nbsig* (igau-1)+3) = epsm(&
                                        nbsig* (igau-1)+3) - (c1*sigma(3)-c2*trsig) - epsflf(3)
        endif
        epspla(nbsig* (igau-1)+4) = epsm( nbsig* (igau-1)+4) - c1* sigma(4) - epsflf(4 )
!
120 end do
!
!
! --- RECUPERATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
!     -------------------------------------------------------------
    call jevech('PDEFOPG', 'E', idefp)
!
! --- AFFECTATION DU VECTEUR EN SORTIE DES DEFORMATIONS PLASTIQUES :
!     ------------------------------------------------------------
! ---    AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
! ---    POINTS D'INTEGRATION :
!        --------------------
    do 140 igau = 1, npg
        do 130 isig = 1, nbsig
            zr(idefp+nbsig* (igau-1)+isig-1) = epspla(nbsig* (igau-1)+ isig)
130     continue
140 end do
end subroutine
