subroutine apvect(sdappa, questz, ip, valr)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sdappa
    integer :: ip
    real(kind=8) :: valr(3)
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INTERROGATION DE LA SDAPPA - VECTEUR (LONGEUER 3)
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUESTI : QUESTION
!              APPARI_TAU1          : TANGENTE 1 AU PT PROJETE
!              APPARI_TAU2          : TANGENTE 2 AU PT PROJETE
!              APPARI_VECTPM        : VECTEUR PT -> PROJECTION
!              APPARI_NOEUD_TAU1    : TANGENTE 1 AU NOEUD
!              APPARI_NOEUD_TAU2    : TANGENTE 2 AU NOEUD
! IN  IP     : INDICE DU POINT OU POSITION DU NOEUD
! OUT VALR   : REPONSE A LA QUESTION
!
!
!
!
    character(len=24) :: apdist, aptau1, aptau2, aptgno
    integer :: jdist, jtau1, jtau2, jptgno
    character(len=24) :: questi
    integer :: posno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SDAPPA
!
    aptau1 = sdappa(1:19)//'.TAU1'
    aptau2 = sdappa(1:19)//'.TAU2'
    apdist = sdappa(1:19)//'.DIST'
    aptgno = sdappa(1:19)//'.TGNO'
!
! --- INITIALISATIONS
!
    valr(1) = 0.d0
    valr(2) = 0.d0
    valr(3) = 0.d0
    questi = questz
!
! --- QUESTION
!
    if (questi .eq. 'APPARI_TAU1') then
        call jeveuo(aptau1, 'L', jtau1)
        valr(1) = zr(jtau1+3*(ip-1)+1-1)
        valr(2) = zr(jtau1+3*(ip-1)+2-1)
        valr(3) = zr(jtau1+3*(ip-1)+3-1)
!
    else if (questi.eq.'APPARI_TAU2') then
        call jeveuo(aptau2, 'L', jtau2)
        valr(1) = zr(jtau2+3*(ip-1)+1-1)
        valr(2) = zr(jtau2+3*(ip-1)+2-1)
        valr(3) = zr(jtau2+3*(ip-1)+3-1)
!
    else if (questi.eq.'APPARI_VECTPM') then
        call jeveuo(apdist, 'L', jdist)
        valr(1) = zr(jdist+4*(ip-1)+2-1)
        valr(2) = zr(jdist+4*(ip-1)+3-1)
        valr(3) = zr(jdist+4*(ip-1)+4-1)
!
    else if (questi.eq.'APPARI_NOEUD_TAU1') then
        call jeveuo(aptgno, 'L', jptgno)
        posno = ip
        valr(1) = zr(jptgno+6*(posno-1)+1-1)
        valr(2) = zr(jptgno+6*(posno-1)+2-1)
        valr(3) = zr(jptgno+6*(posno-1)+3-1)
!
    else if (questi.eq.'APPARI_NOEUD_TAU2') then
        call jeveuo(aptgno, 'L', jptgno)
        posno = ip
        valr(1) = zr(jptgno+6*(posno-1)+4-1)
        valr(2) = zr(jptgno+6*(posno-1)+5-1)
        valr(3) = zr(jptgno+6*(posno-1)+6-1)
!
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
