subroutine nmerro(sderro, sdtime, numins)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmecev.h"
#include "asterfort/nmerge.h"
#include "asterfort/sigusr.h"
#include "asterfort/utexcm.h"
#include "asterfort/utexcp.h"
    integer :: numins
    character(len=24) :: sdtime, sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DES ERREURS ET EXCEPTIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NUMINS : NUMERO DU PAS DE TEMPS
! IN  SDTIME : SD TIMER
! IN  SDERRO : SD GESTION ERREUR
!
! ----------------------------------------------------------------------
!
    character(len=24) :: timpas, timite
    integer :: jtpas, jtite
    real(kind=8) :: rtab(2), r8bid
    integer :: itab(2), ibid
    character(len=8) :: k8bid
    logical :: echldc, echeq1, echeq2, echco1, echco2, echpil
    logical :: mtcpui, mtcpup, itemax
    logical :: echpfg, echpff, echpfc
    logical :: errres
    real(kind=8) :: tpsrst, moyite, moypas
    character(len=16) :: nomevd, action, valk(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
    if (etausr() .eq. 1) call sigusr()
!
! --- ACCES SD TIMER
!
    timpas = sdtime(1:19)//'.TPAS'
    timite = sdtime(1:19)//'.TITE'
    call jeveuo(timpas, 'L', jtpas)
    call jeveuo(timite, 'L', jtite)
!
! --- TEMPS RESTANT
!
    tpsrst = zr(jtpas+1-1)
!
! --- TEMPS MOYENS
!
    moyite = zr(jtite+4-1)
    moypas = zr(jtpas+4-1)
!
! --- RECUPERE LES CODES ERREURS ACTIFS
!
    call nmerge(sderro, 'ERRE_INTE', echldc)
    call nmerge(sderro, 'ERRE_PILO', echpil)
    call nmerge(sderro, 'ERRE_FACS', echeq1)
    call nmerge(sderro, 'ERRE_FACT', echeq2)
    call nmerge(sderro, 'ERRE_CTD1', echco1)
    call nmerge(sderro, 'ERRE_CTD2', echco2)
    call nmerge(sderro, 'ERRE_TIMN', mtcpui)
    call nmerge(sderro, 'ERRE_TIMP', mtcpup)
    call nmerge(sderro, 'ITER_MAXI', itemax)
    call nmerge(sderro, 'ERRE_CTCG', echpfg)
    call nmerge(sderro, 'ERRE_CTCF', echpff)
    call nmerge(sderro, 'ERRE_CTCC', echpfc)
    call nmerge(sderro, 'SOLV_ITMX', errres)
!
! --- LANCEE EXCEPTIONS
!
    if (mtcpui) then
        itab(1) = numins
        rtab(1) = moyite
        rtab(2) = tpsrst
        call utexcm(28, 'MECANONLINE9_1', 0, k8bid, 1,&
                    itab, 2, rtab)
    else if (mtcpup) then
        itab(1) = numins
        rtab(1) = moypas
        rtab(2) = tpsrst
        call utexcm(28, 'MECANONLINE9_2', 0, k8bid, 1,&
                    itab, 2, rtab)
    else if (echldc) then
        call utexcp(23, 'MECANONLINE9_3')
    else if (echeq1.or.echeq2) then
        call utexcp(25, 'MECANONLINE9_4')
    else if (echco1) then
        call utexcp(26, 'MECANONLINE9_5')
    else if (echco2) then
        call utexcp(27, 'MECANONLINE9_6')
    else if (itemax) then
        call utexcp(22, 'MECANONLINE9_7')
    else if (echpil) then
        call utexcp(29, 'MECANONLINE9_8')
    else if (echpfg) then
        call utexcp(30, 'MECANONLINE9_9')
    else if (echpff) then
        call utexcp(31, 'MECANONLINE9_10')
    else if (echpfc) then
        call utexcp(32, 'MECANONLINE9_11')
    else if (errres) then
        call utexcp(35, 'MECANONLINE9_12')
    else
        call nmecev(sderro, 'L', nomevd, action)
        valk(1) = action
        valk(2) = nomevd
        if (action .eq. 'ARRET') then
            call utexcm(33, 'MECANONLINE9_51', 1, nomevd, 0,&
                        ibid, 0, r8bid)
        else
            call utexcm(34, 'MECANONLINE9_50', 2, valk, 0,&
                        ibid, 0, r8bid)
        endif
    endif
!
    call jedema()
!
end subroutine
