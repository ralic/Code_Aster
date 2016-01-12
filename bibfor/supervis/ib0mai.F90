subroutine ib0mai()
    use superv_module, only: superv_before
    implicit none
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     MAIN D'ANALYSE DE LA COMMANDE DE DEMARRAGE
!     ------------------------------------------------------------------
!     UTILISATION DU COMMON POUR L'AFFICHAGE
#include "asterc/gtopti.h"
#include "asterc/gtoptr.h"
#include "asterc/ismaem.h"
#include "asterc/loisem.h"
#include "asterfort/entete.h"
#include "asterfort/ib1mai.h"
#include "asterfort/ibimpr.h"
#include "asterfort/jedebu.h"
#include "asterfort/jeinif.h"
#include "asterfort/r8inir.h"
#include "asterfort/utgtme.h"
#include "asterfort/utmess.h"
    integer :: lfic, mfic
    common /fenvje/  lfic,mfic
!
    integer :: nexcep
    common /utexc /  nexcep
!     ------------------------------------------------------------------
    character(len=8) :: nomf, k8tab(4)
    integer :: unmega, idebug, iret, lois
    integer :: mxdyn, ibid
    real(kind=8) :: valr(4), moctet, memory, sizf
!
    call ib1mai()
    nexcep = 0
    ibid = 0
!
    call r8inir(4, 0.d0, valr, 1)
    k8tab(1) = 'VMSIZE'
    call utgtme(1, k8tab, valr, iret)
!     --- MEMOIRE POUR LE GESTIONNAIRE D'OBJET ---
    unmega = 1024 * 1024
!     RESTRICTION POUR UNE TAILLE MEMOIRE JEVEUX EXACTE
    lois = loisem()
! --- UNITES LOGIQUES
    call ibimpr()
!
!     --- OUVERTURE DE GESTIONNAIRE D'OBJET ---
    idebug = 0
    call gtopti('dbgjeveux', idebug, iret)
!
    memory = 0.d0
    call gtoptr('memory', memory, iret)
    moctet = memory * unmega
    if (moctet .gt. ismaem()) then
        valr(1) = moctet
        valr(2) = ismaem()
        call utmess('F', 'JEVEUX_1', nr=2, valr=valr)
    endif
    mxdyn = int(moctet)
!
    call jedebu(4, mxdyn/lois, idebug)
!
!     --- ALLOCATION D'UNE BASE DE DONNEES TEMPORAIRE VOLATILE---
    nomf = 'VOLATILE'
    call jeinif('DEBUT', 'DETRUIT', nomf, 'V', 250,&
                100, 1)
    call superv_before()
! --- IMPRESSION DE L'ENTETE
    call entete()
    k8tab(1) = 'LIMIT_JV'
    k8tab(2) = 'MEM_TOTA'
    k8tab(3) = 'MEM_INIT'
    k8tab(4) = 'MEM_JDC'
    call utgtme(4, k8tab, valr, iret)
    call utmess('I', 'SUPERVIS2_22', nr=4, valr=valr)
!
    sizf = mfic/(1024*1024.0d0)
    call utmess('I', 'SUPERVIS2_24', sr=sizf)
!
    if (idebug .eq. 1) then
        call utmess('I', 'SUPERVIS_12')
    endif
end subroutine
