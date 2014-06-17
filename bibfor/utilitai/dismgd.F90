subroutine dismgd(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(GRANDEUR)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=24) :: questl
    character(len=32) :: repk
    character(len=8) :: nomob
    character(len=*) :: repkz, nomobz
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UNE GRANDEUR
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
!
!-----------------------------------------------------------------------
    integer :: iadgd, iancmp,  ibid, icode, igdco, igdli
    integer :: nmax, numgd
    character(len=8), pointer :: typegd(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    questl = questi
!
!
    if (questl(1:7) .eq. 'NUM_GD ') then
        call jenonu(jexnom('&CATA.GD.NOMGD', nomob), repi)
        goto 9999
    endif
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomob), ibid)
    call jeveuo(jexnum('&CATA.GD.DESCRIGD', ibid), 'L', iadgd)
    icode=zi(iadgd)
!
    if (questi(1:12) .eq. 'TYPE_MATRICE') then
        if (icode .le. 3) then
            repk=' '
        else if (icode.eq.4) then
            repk='SYMETRI'
        else if (icode.eq.5) then
            repk='NON_SYM'
        endif
!
    else if (questi(1:9).eq.'NUM_GD_SI') then
        if (icode .eq. 1) then
            call jenonu(jexnom('&CATA.GD.NOMGD', nomob), repi)
        else if (icode.eq.3) then
            repi=zi(iadgd-1+4)
        else if (icode.eq.4) then
            repi=zi(iadgd-1+4)
        else if (icode.eq.5) then
            igdli=zi(iadgd-1+4)
            igdco=zi(iadgd-1+5)
            if (igdli .ne. igdco) then
                call utmess('F', 'UTILITAI_57')
                ierd=1
                goto 9999
            else
                repi=igdli
            endif
        else
            ASSERT(.false.)
        endif
!
    else if (questi(1:9).eq.'NOM_GD_SI') then
        if (icode .eq. 5) then
            igdli=zi(iadgd-1+4)
            igdco=zi(iadgd-1+5)
            if (igdli .ne. igdco) then
                call utmess('F', 'UTILITAI_59')
                ierd=1
                goto 9999
            else
                numgd=igdli
                call jenuno(jexnum('&CATA.GD.NOMGD', numgd), repk)
            endif
        else if (icode.le.2) then
            repk=nomob
        else
            numgd=zi(iadgd-1+4)
            call jenuno(jexnum('&CATA.GD.NOMGD', numgd), repk)
        endif
!
    else if (questi.eq.'NB_EC') then
        if (icode .ge. 3) then
            call utmess('F', 'UTILITAI_60')
            ierd=1
            goto 9999
        endif
        repi= zi(iadgd-1+3)
!
        else if ((questi.eq.'NB_CMP_MAX') .or.(questi.eq.'NU_CMP_LAGR'))&
    then
        if (icode .ge. 3) then
            call utmess('F', 'UTILITAI_60')
            ierd=1
            goto 9999
        endif
        call jelira(jexnom('&CATA.GD.NOMCMP', nomob), 'LONMAX', nmax)
        if (questi .eq. 'NB_CMP_MAX') then
            repi=nmax
        else if (questi.eq.'NU_CMP_LAGR') then
            call jeveuo(jexnom('&CATA.GD.NOMCMP', nomob), 'L', iancmp)
            repi=indik8(zk8(iancmp),'LAGR',1,nmax)
        else
            ASSERT(.false.)
        endif
!
    else if (questi.eq.'TYPE_SCA') then
        call jeveuo('&CATA.GD.TYPEGD', 'L', vk8=typegd)
        call jenonu(jexnom('&CATA.GD.NOMGD', nomob), numgd)
        repk= typegd(numgd)
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
