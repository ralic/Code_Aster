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
    implicit none
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmecev.h"
#include "asterfort/nmerge.h"
#include "asterfort/sigusr.h"
#include "asterfort/utmess.h"
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
        call utmess('Z', 'MECANONLINE9_1', si=itab(1), nr=2, valr=rtab,&
                    num_except=28)
    else if (mtcpup) then
        itab(1) = numins
        rtab(1) = moypas
        rtab(2) = tpsrst
        call utmess('Z', 'MECANONLINE9_2', si=itab(1), nr=2, valr=rtab,&
                    num_except=28)
    else if (echldc) then
        call utmess('Z', 'MECANONLINE9_3', num_except=23)
    else if (echeq1.or.echeq2) then
        call utmess('Z', 'MECANONLINE9_4', num_except=25)
    else if (echco1) then
        call utmess('Z', 'MECANONLINE9_5', num_except=26)
    else if (echco2) then
        call utmess('Z', 'MECANONLINE9_6', num_except=27)
    else if (itemax) then
        call utmess('Z', 'MECANONLINE9_7', num_except=22)
    else if (echpil) then
        call utmess('Z', 'MECANONLINE9_8', num_except=29)
    else if (echpfg) then
        call utmess('Z', 'MECANONLINE9_9', num_except=30)
    else if (echpff) then
        call utmess('Z', 'MECANONLINE9_10', num_except=31)
    else if (echpfc) then
        call utmess('Z', 'MECANONLINE9_11', num_except=32)
    else if (errres) then
        call utmess('Z', 'MECANONLINE9_12', num_except=35)
    else
        call nmecev(sderro, 'L', nomevd, action)
        valk(1) = action
        valk(2) = nomevd
        if (action .eq. 'ARRET') then
            call utmess('Z', 'MECANONLINE9_51', sk=nomevd, num_except=33)
        else
            call utmess('Z', 'MECANONLINE9_50', nk=2, valk=valk, num_except=34)
        endif
    endif
!
    call jedema()
!
end subroutine
