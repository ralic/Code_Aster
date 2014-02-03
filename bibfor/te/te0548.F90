subroutine te0548(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/elelin.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/xjacf2.h"
#include "asterfort/xjacff.h"
#include "asterfort/xlacti.h"
#include "asterfort/xminte.h"
#include "asterfort/xmoffc.h"
#include "asterfort/xplmat.h"
#include "asterfort/xteini.h"
    character(len=16) :: option, nomte
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!.......................................................................
!
!                CONTACT X-FEM METHODE CONTINUE :
!             MISE A JOUR DU SEUIL DE FROTTEMENT
!             MISE A JOUR DE LA COHESION DANS LE CAS COHESIF
!
!
!  OPTION : 'XREACL' (X-FEM MISE À JOUR DU SEUIL DE FROTTEMENT)
!
!  ENTREES  ---> OPTION : OPTION DE CALCUL
!           ---> NOMTE  : NOM DU TYPE ELEMENT
!
!......................................................................
!
!
    integer :: i, j, ifa, ipgf, isspg, ni, pli
    integer :: ideppl, jaint, jcface, jlonch, jseuil, ipoidf, ivff, idfdef
    integer :: iadzi, iazk24, ipoids, ivf, idfde, jgano, jdonco
    integer :: ndim, nno, nnos, npg, nfh, ddlc, nnom, integ, ninter, nfe
    integer :: nface, cface(5, 3), ibid, nnof, npgf, jptint
    integer :: singu, jbasec, nptf
    integer :: ddls, nddl, nnol, lact(8), nlact, igeom, ddlm
    integer :: contac
    character(len=8) :: elref, typma, fpg, elc, elrefc
    real(kind=8) :: seuil, ffi, g(3), rbid, ffp(27), ffc(8), nd(3)
    real(kind=8) :: ffpc(27), dfbid(27, 3), r3bid(3)
!......................................................................
!
    call jemarq()
!
    call elref1(elref)
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!     INITIALISATION DES DIMENSIONS DES DDLS X-FEM
    call xteini(nomte, nfh, nfe, singu, ddlc,&
                nnom, ddls, nddl, ddlm, ibid,&
                contac)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
! --- ROUTINE SPECIFIQUE P2P1
!
    call elelin(contac, elref, elrefc, ibid, ibid)
!
!     DEP ACTUEL (DEPPLU) : 'PDEPL_P'
    call jevech('PDEPL_P', 'L', ideppl)
    call jevech('PDONCO', 'L', jdonco)
    call jevech('PAINTER', 'L', jaint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONGCO', 'L', jlonch)
    call jevech('PPINTER', 'L', jptint)
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PSEUIL', 'E', jseuil)
    call jevech('PBASECO', 'L', jbasec)
!
!     RECUPERATIONS DES DONNEES SUR LE CONTACT ET
!     SUR LA TOPOLOGIE DES FACETTES
    ninter=zi(jlonch-1+1)
    nface=zi(jlonch-1+2)
    nptf=zi(jlonch-1+3)
!
    do 15 i = 1, nface
        do 16 j = 1, nptf
            cface(i,j)=zi(jcface-1+nptf*(i-1)+j)
16      continue
15  end do
!
!     SCHEMA D'INTEGRATION NUMERIQUE ET ELEMENT DE REFERENCE DE CONTACT
    integ = nint(zr(jdonco-1+4))
    call xminte(ndim, integ, fpg)
!
    if (ndim .eq. 3) then
        elc='TR3'
    else if (ndim.eq.2) then
        if (contac .le. 2) then
            elc='SE2'
        else
            elc='SE3'
        endif
    endif
!
    call elref4(elc, fpg, ibid, nnof, ibid,&
                npgf, ipoidf, ivff, idfdef, ibid)
!
    call tecael(iadzi, iazk24)
    typma=zk24(iazk24-1+3+zi(iadzi-1+2)+3)
!
!     LISTE DES LAMBDAS ACTIFS
!
    call xlacti(typma, ninter, jaint, lact, nlact)
    if (contac .eq. 1) nnol=nno
    if (contac .eq. 3) nnol=nnos
!
!     BOUCLE SUR LES FACETTES
    do 100 ifa = 1, nface
!
!       BOUCLE SUR LES POINTS DE GAUSS DES FACETTES
        do 110 ipgf = 1, npgf
!
!         INDICE DE CE POINT DE GAUSS DANS PSEUIL
            isspg=npgf*(ifa-1)+ipgf
!
!         CALCUL DE JAC (PRODUIT DU JACOBIEN ET DU POIDS)
!         ET DES FF DE L'ELEMENT PARENT AU POINT DE GAUSS
!         ET LA NORMALE ND ORIENTÉE DE ESCL -> MAIT
            if (ndim .eq. 3) then
                call xjacff(elref, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, ipgf, nno,&
                            igeom, jbasec, g, rbid, ffp,&
                            ffpc, dfbid, nd, r3bid, r3bid)
            else if (ndim.eq.2) then
                call xjacf2(elref, elrefc, elc, ndim, fpg,&
                            jptint, ifa, cface, nptf, ipgf,&
                            nno, igeom, jbasec, g, rbid,&
                            ffp, ffpc, dfbid, nd, r3bid)
            endif
!
!        CALCUL DES FONCTIONS DE FORMES DE CONTACT DANS LE CAS LINEAIRE
            if (contac .eq. 1) then
                call xmoffc(lact, nlact, nno, ffp, ffc)
            else if (contac.eq.3) then
                call xmoffc(lact, nlact, nnos, ffpc, ffc)
            else if (contac.eq.4) then
                call xmoffc(lact, nlact, nno, ffp, ffc)
            endif
!
!         CALCUL DU NOUVEAU SEUIL A PARTIR DES LAMBDA DE DEPPLU
            seuil = 0.d0
            do 120 i = 1, nnol
                ffi=ffc(i)
                ni=i
                call xplmat(ndim, nfh, nfe, ddlc, ddlm,&
                            nno, nnom, ni, pli)
                seuil = seuil + ffi * zr(ideppl-1+pli)
120          continue
!
!         LORS D'UNE CONVERGENCE FORCEE, IL SE PEUT QUE LES REACTIONS
!         SOIENT TROP PETITES. LE POINT DOIT ETRE CONSIDERE GLISSANT.
            if (abs(seuil) .lt. 1.d-11) then
                seuil=0.d0
            endif
!
            zr(jseuil-1+isspg)=seuil
!
110      continue
100  end do
!
    call jedema()
end subroutine
