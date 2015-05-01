subroutine dismnu(questi, nomobz, repi, repkz, ierd)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     --     DISMOI(NUME_DDL) (OU PARFOIS NUME_DDL_GENE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/dismgd.h"
#include "asterfort/dismlg.h"
#include "asterfort/dismpn.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
!
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=24) :: questl
    character(len=32) :: repk
    character(len=14) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE NUME_DDL (K14)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: nomlig
!
!      ATTENTION: POUR UN NUME_DDL_GENE
!
!         NOMLIG = 'LIAISONS'
!         CE N'EST PAS LE NOM D'UN LIGREL
!
!-----------------------------------------------------------------------
    integer ::  iarefe, iret, jnslv, jslvk
    integer, pointer :: nequ(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    questl = questi
!
    if (questl(1:7) .eq. 'NOM_GD ') then
        call jeveuo(nomob//'.NUME.REFN', 'L', iarefe)
        repk = zk24(iarefe+1) (1:8)
!
    else if (questi(1:9).eq.'NUM_GD_SI') then
        call jeveuo(nomob//'.NUME.REFN', 'L', iarefe)
        call dismgd(questi, zk24(iarefe+1) (1:8), repi, repk, ierd)
!
    else if (questi.eq.'NB_EQUA') then
        call jeveuo(nomob//'.NUME.NEQU', 'L', vi=nequ)
        repi = nequ(1)
!
    else if (questi.eq.'PROF_CHNO') then
        repk = nomob//'.NUME'
!
    else if (questi.eq.'NOM_MODELE') then
        call dismpn(questi, nomob//'.NUME', repi, repk, ierd)
!
    else if (questi.eq.'PHENOMENE') then
        call jenuno(jexnum(nomob//'.NUME.LILI', 2), nomlig)
        if (nomlig(1:8) .eq. 'LIAISONS') then
            repk = 'MECANIQUE'
        else
            call dismlg(questi, nomlig, repi, repk, ierd)
        endif
!
    else if (questi.eq.'NOM_MAILLA') then
        call jeveuo(nomob//'.NUME.REFN', 'L', iarefe)
        repk = zk24(iarefe) (1:8)
!
    else if (questi.eq.'SOLVEUR') then
        repk='XXXX'
        call jeexin(nomob//'.NSLV', iret)
        if (iret .gt. 0) then
            call jeveuo(nomob//'.NSLV', 'L', jnslv)
            repk=zk24(jnslv)(1:19)
        endif
!
    else if (questi.eq.'METH_RESO') then
        repk='XXXX'
        call jeexin(nomob//'.NSLV', iret)
        if (iret .gt. 0) then
            call jeveuo(nomob//'.NSLV', 'L', jnslv)
            if (zk24(jnslv) .ne. ' ') then
                call jeveuo(zk24(jnslv)(1:19)//'.SLVK', 'L', jslvk)
                repk=zk24(jslvk-1+1)
            endif
        endif
!
    else if (questi.eq.'RENUM_RESO') then
        repk='XXXX'
        call jeexin(nomob//'.NSLV', iret)
        if (iret .gt. 0) then
            call jeveuo(nomob//'.NSLV', 'L', jnslv)
            if (zk24(jnslv) .ne. ' ') then
                call jeveuo(zk24(jnslv)(1:19)//'.SLVK', 'L', jslvk)
                repk=zk24(jslvk-1+4)
            endif
        endif
!
    else
        ierd = 1
    endif
!
    repkz = repk
    call jedema()
end subroutine
