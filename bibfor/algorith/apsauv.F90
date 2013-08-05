subroutine apsauv(phasez, sdappa, izone, ip, vali,&
                  valr)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/appari.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: phasez
    character(len=19) :: sdappa
    integer :: izone, ip
    integer :: vali(*)
    real(kind=8) :: valr(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! SAUVEGARDE APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  PHASE  : PHASE DE STOCKAGE
!                'ND_PROCHE' - NOEUD LE PLUS PROCHE DU POINT
!                'MA_PROCHE' - MAILLE LA PLUS PROCHE DU POINT
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  IZONE  : NUMERO DE LA ZONE
! IN  IP     : NUMERO DU POINT APPARIE
! IN  VALI   : INFO. DE TYPE INTEGER A STOCKER
!                'ND_PROCHE'
!                   VALI(1) =  1  SI APPARIEMENT
!                             -1  SI POINT DANS SANS_NOEUD
!                             -2  SI AUCUN NOEUD DANS TOLE_APPA
!                   VALI(2) = POSNO POSITION NOEUD (DANS SD) APPARIE
!                              0  SI PAS DE NOEUD APPARIE
!                'MA_PROCHE'
!                   VALI(1) =  2  SI APPARIEMENT
!                             -3  SI POINT HORS MAILLE
!                   VALI(2) = POSMA POSITION MAILLE (DANS SD) APPARIEE
!                               0 SI PAS DE MAILLE APPARIEE
!
! IN  VALR   : INFO. DE TYPE REAL*8 A STOCKER
!                'ND_PROCHE'
!                   VALR(1) = DIST DISTANCE DU NOEUD APPARIE
!                   VALR(2) = VECTEUR PT -> PROJETE
!                   VALR(3) = VECTEUR PT -> PROJETE
!                   VALR(4) = VECTEUR PT -> PROJETE
!                'MA_PROCHE'
!                   VALR(1) = DIST DISTANCE DE LA MAILLE
!                   VALR(2) = KSI1 COORD. PARAM. DE LA PROJ. SUR MAILLE
!                   VALR(3) = KSI3 COORD. PARAM. DE LA PROJ. SUR MAILLE
!                   VALR(4) = TAU1 TANGENTE PROJ. SUR MAILLE
!                   VALR(5) = TAU1 TANGENTE PROJ. SUR MAILLE
!                   VALR(6) = TAU1 TANGENTE PROJ. SUR MAILLE
!                   VALR(7) = TAU2 TANGENTE PROJ. SUR MAILLE
!                   VALR(8) = TAU2 TANGENTE PROJ. SUR MAILLE
!                   VALR(9) = TAU2 TANGENTE PROJ. SUR MAILLE
!                   VALR(10) = VECTEUR PT -> PROJETE
!                   VALR(11) = VECTEUR PT -> PROJETE
!                   VALR(12) = VECTEUR PT -> PROJETE
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: phase
    character(len=24) :: appar
    integer :: jappa
    character(len=24) :: apdist, aptau1, aptau2, approj
    integer :: jdist, jtau1, jtau2, jproj
    integer :: ntpt
    integer :: posno, posma
    integer :: typapp
    real(kind=8) :: dist
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    appar = sdappa(1:19)//'.APPA'
    call jeveuo(appar, 'E', jappa)
    apdist = sdappa(1:19)//'.DIST'
    aptau1 = sdappa(1:19)//'.TAU1'
    aptau2 = sdappa(1:19)//'.TAU2'
    call jeveuo(apdist, 'E', jdist)
    call jeveuo(aptau1, 'E', jtau1)
    call jeveuo(aptau2, 'E', jtau2)
    approj = sdappa(1:19)//'.PROJ'
    call jeveuo(approj, 'E', jproj)
!
! --- INFOS SDAPPA
!
    call appari(sdappa, 'APPARI_NTPT', ntpt)
!
! --- INITIALISATIONS
!
    phase = phasez
    typapp = 0
    posno = 0
    posma = 0
    ASSERT(ip.le.ntpt)
!
! --- SAUVEGARDE INFOS NOEUD LE PLUS PROCHE
!
    if (phase .eq. 'ND_PROCHE') then
!
! ----- TYPE D'APPARIEMENT
!
        typapp = vali(1)
        ASSERT((typapp.eq.-2).or.(typapp.eq.-1).or. (typapp.eq.1))
!
! ----- INDICE ET DISTANCE DU NOEUD LE PLUS PROCHE
!
        posno = vali(2)
        dist = valr(1)
!
! ----- SAUVEGARDE
!
        zi(jappa+4*(ip-1)+1-1) = typapp
        zi(jappa+4*(ip-1)+2-1) = posno
        zi(jappa+4*(ip-1)+3-1) = izone
        zr(jdist+4*(ip-1)+1-1) = dist
        zr(jdist+4*(ip-1)+2-1) = valr(2)
        zr(jdist+4*(ip-1)+3-1) = valr(3)
        zr(jdist+4*(ip-1)+4-1) = valr(4)
!
! --- SAUVEGARDE INFOS MAILLE LA PLUS PROCHE
!
    else if (phase.eq.'MA_PROCHE') then
!
! --- TYPE D'APPARIEMENT
!
        typapp = vali(1)
!
! --- INDICE ET DISTANCE DE LA MAILLE LA PLUS PROCHE
!
        posma = vali(2)
        dist = valr(1)
!
! --- SAUVEGARDE
!
        zi(jappa+4*(ip-1)+1-1) = typapp
        zi(jappa+4*(ip-1)+2-1) = posma
        zi(jappa+4*(ip-1)+3-1) = izone
        zr(jdist+4*(ip-1)+1-1) = dist
        zr(jdist+4*(ip-1)+2-1) = valr(10)
        zr(jdist+4*(ip-1)+3-1) = valr(11)
        zr(jdist+4*(ip-1)+4-1) = valr(12)
        zr(jproj+2*(ip-1)+1-1) = valr(2)
        zr(jproj+2*(ip-1)+2-1) = valr(3)
        zr(jtau1+3*(ip-1)+1-1) = valr(4)
        zr(jtau1+3*(ip-1)+2-1) = valr(5)
        zr(jtau1+3*(ip-1)+3-1) = valr(6)
        zr(jtau2+3*(ip-1)+1-1) = valr(7)
        zr(jtau2+3*(ip-1)+2-1) = valr(8)
        zr(jtau2+3*(ip-1)+3-1) = valr(9)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
