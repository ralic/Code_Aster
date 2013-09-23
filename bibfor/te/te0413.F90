subroutine te0413(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/crgdm.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elref5.h"
#include "asterfort/gquad4.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
    character(len=16) :: option, nomte
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTIONS REALISEES:
!
!      CALCUL DE LA DENSITE DE DISSIPATION
!      A L'EQUILIBRE POUR LES ELEMENTS DKTG ET LA LOI GLRC_DM
!      .SOIT AUX POINTS D'INTEGRATION : OPTION 'DISS_ELGA'
!      .SOIT L INTEGRALE PAR ELEMENT  : OPTION 'DISS_ELEM'
!
!      OPTIONS : 'DISS_ELGA'
!                'DISS_ELEM'
!
! ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: npgmx
    parameter (npgmx=4)
!
    real(kind=8) :: pgl(3, 3)
    real(kind=8) :: qsi, eta, xyzl(3, 4), jacob(5), poids, cara(25)
    real(kind=8) :: disse(npgmx), dse
    real(kind=8) :: r8b(15), ep, seuil
!
    integer :: ndim, nno, nnoel, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jgeom, ipg, idener, imate
    integer :: icompo, icacoq, jvari, nbvar, jtab(7)
    integer :: iret
!
    character(len=16) :: valk(2)
    logical :: dkq, lkit, lbid
!
    if (nomte(1:8) .eq. 'MEDKQG4 ') then
        dkq = .true.
    else if (nomte(1:8).eq.'MEDKTG3 ') then
        dkq = .false.
    else
        call utmess('F', 'ELEMENTS_34', sk=nomte)
    endif
!
    call elref5(' ', 'RIGI', ndim, nno, nnoel,&
                npg, ipoids, icoopg, ivf, idfdx,&
                idfd2, jgano)
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCOMPOR', 'L', icompo)
    if (nno .eq. 3) then
        call dxtpgl(zr(jgeom), pgl)
    else if (nno.eq.4) then
        call dxqpgl(zr(jgeom), pgl, 'S', iret)
    endif
!
    lkit = zk16(icompo)(1:7).eq.'KIT_DDI'
!
    if ((zk16(icompo)(1:7).eq.'GLRC_DM') .or. (lkit.and.(zk16(icompo+7)(1:7).eq.'GLRC_DM'))) then
!
        call jevech('PCACOQU', 'L', icacoq)
!
        call utpvgl(nno, 3, pgl, zr(jgeom), xyzl)
!
        if (dkq) then
            call gquad4(xyzl, cara)
        else
            call gtria3(xyzl, cara)
        endif
!
        if (option .eq. 'DISS_ELGA') then
            call tecach('OON', 'PVARIGR', 'L', iret, nval=7,&
                        itab=jtab)
            jvari = jtab(1)
        else if (option.eq.'DISS_ELEM') then
            call tecach('OON', 'PVARIPR', 'L', iret, nval=7,&
                        itab=jtab)
            jvari = jtab(1)
        endif
!
        call r8inir(npgmx, 0.d0, disse, 1)
        dse = 0.0d0
!
        read (zk16(icompo-1+2),'(I16)') nbvar
        ep = zr(icacoq)
!
! ---- BOUCLE SUR LES POINTS D'INTEGRATION :
!      ===================================
        do 20 ipg = 1, npg
!
            qsi = zr(icoopg-1+ndim*(ipg-1)+1)
            eta = zr(icoopg-1+ndim*(ipg-1)+2)
            if (dkq) then
                call jquad4(xyzl, qsi, eta, jacob)
                poids = zr(ipoids+ipg-1)*jacob(1)
            else
                poids = zr(ipoids+ipg-1)*cara(7)
            endif
!
            call jevech('PMATERC', 'L', imate)
!
            call crgdm(zi(imate), 'GLRC_DM         ', r8b(1), r8b(2), r8b(3),&
                       r8b(4), r8b(5), r8b(6), r8b(7), seuil,&
                       r8b(8), r8b(9), ep, .false., ipg,&
                       lbid, r8b(10), r8b(11), r8b(12), r8b(13),&
                       r8b(14), r8b(15))
!
!  --    CALCUL DE LA DENSITE D'ENERGIE POTENTIELLE ELASTIQUE :
!        ==========================================================
            if ((option.eq.'DISS_ELGA') .or. (option.eq.'DISS_ELEM')) then
!
                disse(ipg) = (zr( jvari-1 + (ipg-1)*nbvar + 1) + zr(jvari-1 + (ipg-1)*nbvar + 2 )&
                             )*seuil
!
                dse = dse + disse(ipg)*poids
!
            endif
!
20      end do
!
! ---- RECUPERATION DU CHAMP DES DENSITES D'ENERGIE DE DEFORMATION
! ---- ELASTIQUE EN SORTIE
!      -------------------
        if (option .eq. 'DISS_ELGA') then
            call jevech('PDISSPG', 'E', idener)
        else if (option.eq.'DISS_ELEM') then
            call jevech('PDISSD1', 'E', idener)
        endif
!
! --- OPTIONS DISS_ELGA
!     ==============================
        if (option .eq. 'DISS_ELGA') then
            do 100 ipg = 1, npg
                zr(idener-1+(ipg-1)*1 +1) = disse(ipg)
100          continue
!
! --- OPTION DISS_ELEM
!     ================
        else if (option.eq.'DISS_ELEM') then
            zr(idener-1+1) = dse
        endif
!
    else
!      RELATION NON PROGRAMMEE
        valk(1) = option
        valk(2) = zk16(icompo)(1:7)
        call utmess('A', 'ELEMENTS4_63', nk=2, valk=valk)
    endif
!
end subroutine
