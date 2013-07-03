subroutine dismli(questi, nomobz, repi, repkz, ierd)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     --     DISMOI(INTERF_DYNA)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=14) :: nomob
    character(len=*) :: repkz, nomobz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE INTERF_DYNA (K14)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
    integer :: lldes, llref
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
!
    if (questi(1:10) .eq. 'NOM_MAILLA') then
        call jeveuo(nomob(1:8)//'.IDC_REFE', 'L', llref)
        repk(1:8)=zk24(llref)
    else if (questi(1:12).eq.'NOM_NUME_DDL') then
        call jeveuo(nomob(1:8)//'.IDC_REFE', 'L', llref)
        repk(1:19)=zk24(llref+1)
    else if (questi.eq.'NOM_MODE_CYCL') then
        call jeveuo(nomob(1:8)//'.IDC_REFE', 'L', llref)
        repk(1:8)=zk24(llref+2)
    else if (questi(1:5).eq.'NB_EC') then
        call jeveuo(nomob(1:8)//'.IDC_DESC', 'L', lldes)
        repi=zi(lldes+1)
    else if (questi(1:10).eq.'NB_CMP_MAX') then
        call jeveuo(nomob(1:8)//'.IDC_DESC', 'L', lldes)
        repi=zi(lldes+2)
    else if (questi(1:6).eq.'NUM_GD') then
        call jeveuo(nomob(1:8)//'.IDC_DESC', 'L', lldes)
        repi=zi(lldes+3)
    else
        ierd=1
    endif
!
    repkz = repk
    call jedema()
end subroutine
