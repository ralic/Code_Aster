subroutine apinfr(sdappa, questz, ip, valr)
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/infdbg.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=19) :: sdappa
    integer :: ip
    real(kind=8) :: valr
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! INTERROGATION DE LA SDAPPA - REEL
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  QUESTI : QUESTION
!              APPARI_PROJ_KSI1 : COORD. PARAM. 1 PROJECTION DU PT
!              APPARI_PROJ_KSI2 : COORD. PARAM. 2 PROJECTION DU PT
!              APPARI_DIST      : DISTANCE PT - PROJECTION
! IN  IP     : INDICE DU POINT
! OUT VALR   : REPONSE A LA QUESTION
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: apdist, approj
    integer :: jdist, jproj
    character(len=16) :: questi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('APPARIEMENT', ifm, niv)
!
! --- ACCES SDAPPA
!
    approj = sdappa(1:19)//'.PROJ'
    apdist = sdappa(1:19)//'.DIST'
!
! --- INITIALISATIONS
!
    valr = 0.d0
    questi = questz
!
! --- QUESTION
!
    if (questi .eq. 'APPARI_PROJ_KSI1') then
        call jeveuo(approj, 'L', jproj)
        valr = zr(jproj+2*(ip-1)+1-1)
    else if (questi.eq.'APPARI_PROJ_KSI2') then
        call jeveuo(approj, 'L', jproj)
        valr = zr(jproj+2*(ip-1)+2-1)
!
    else if (questi.eq.'APPARI_DIST') then
        call jeveuo(apdist, 'L', jdist)
        valr = zr(jdist+4*(ip-1)+1-1)
!
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
