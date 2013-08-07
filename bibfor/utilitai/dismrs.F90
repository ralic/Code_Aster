subroutine dismrs(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(RESULTAT)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismcp.h"
#include "asterfort/dismrc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsdocu.h"
#include "asterfort/rslipa.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=32) :: repk
    character(len=19) :: nomob
    character(len=*) :: nomobz, repkz
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE RESULTAT
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=24) :: objdes
    character(len=4) :: docu
    character(len=24) :: valk(2)
    character(len=8) :: k8bid
    character(len=19) :: nomch
    complex(kind=8) :: cbid
    integer :: ibid
    real(kind=8) :: rbid
!
!-----------------------------------------------------------------------
    integer :: i, iad, iatach, ico, iexi, iret, j
    integer :: jlipar, k, n1, nbch, nbdyn, nbmod, nbstat
    integer :: nbsy
!-----------------------------------------------------------------------
    call jemarq()
    nomob = nomobz
    repi = 0
    repk = ' '
    ierd = 0
!
!
    if (questi .eq. 'TYPE_RESU') then
!         ---------------------
        call jeexin(nomob//'.DESC', ibid)
        if (ibid .gt. 0) then
            objdes=nomob//'.DESC'
        else
            objdes=nomob//'.CELD'
        endif
!
        call jelira(objdes, 'GENR', cval=k8bid)
        if (k8bid(1:1) .eq. 'N') then
            call jelira(objdes, 'DOCU', cval=docu)
            call rsdocu(docu, repk, iret)
            if (iret .ne. 0) then
                valk(1) = docu
                valk(2) = nomob
                call u2mesk('F', 'UTILITAI_68', 2, valk)
                ierd=1
                goto 9999
            endif
        else
            repk = 'CHAMP'
        endif
!
!
!
        else if ((questi.eq.'NOM_MODELE').or. (questi.eq.'MODELE').or.&
    (questi.eq.'MODELE_1').or. (questi.eq.'CHAM_MATER').or. (&
    questi.eq.'CHAM_MATER_1').or. (questi.eq.'CARA_ELEM').or. (&
    questi.eq.'CARA_ELEM_1')) then
!     ------------------------------------------
        if ((questi.eq.'NOM_MODELE') .or. (questi(1:6).eq.'MODELE')) then
            call rslipa(nomob, 'MODELE', '&&DISMRS.LIPAR', jlipar, n1)
        else if (questi(1:9).eq.'CARA_ELEM') then
            call rslipa(nomob, 'CARAELEM', '&&DISMRS.LIPAR', jlipar, n1)
        else if (questi(1:10).eq.'CHAM_MATER') then
            call rslipa(nomob, 'CHAMPMAT', '&&DISMRS.LIPAR', jlipar, n1)
        endif
        ASSERT(n1.ge.1)
        repk=' '
        ico=0
        do 10, k=1,n1
        if (zk8(jlipar-1+k) .ne. ' ') then
            if (zk8(jlipar-1+k) .ne. repk) then
                ico=ico+1
                repk=zk8(jlipar-1+k)
            endif
        endif
10      continue
        if (ico .eq. 0) repk='#AUCUN'
        if (ico .gt. 1) then
            if ((questi.eq.'MODELE_1') .or. (questi.eq.'CARA_ELEM_1') .or.&
                (questi.eq.'CHAM_MATER_1')) then
!           REPK=REPK
            else
                repk='#PLUSIEURS'
            endif
        endif
        call jedetr('&&DISMRS.LIPAR')
!
!
!
    else if (questi.eq.'NOM_MAILLA') then
!     ------------------------------------------
        call jelira(jexnum(nomob//'.TACH', 1), 'LONMAX', nbch)
        call jeveuo(jexnum(nomob//'.TACH', 1), 'L', iatach)
        do 1, i=1,nbch
        nomch=zk24(iatach-1+i)(1:19)
        if (nomch(1:1) .ne. ' ') then
            call dismcp(questi, nomch, repi, repk, ierd)
            goto 9999
        endif
 1      continue
!
!        -- SINON ON PARCOURT TOUS LES CHAMPS DU RESULTAT :
        call jelira(nomob//'.TACH', 'NMAXOC', nbsy)
        do 2, j=2,nbsy
        call jelira(jexnum(nomob//'.TACH', j), 'LONMAX', nbch)
        call jeveuo(jexnum(nomob//'.TACH', j), 'L', iatach)
        do 3, i=1,nbch
        nomch=zk24(iatach-1+i)(1:19)
        if (nomch(1:1) .ne. ' ') then
            call dismcp(questi, nomch, repi, repk, ierd)
            goto 9999
        endif
 3      continue
 2      continue
        call u2mess('F', 'UTILITAI_69')
        ierd=1
!
!
    else if (questi.eq.'EXI_CHAM_ELEM') then
!     ------------------------------------------
        call jelira(nomob//'.TACH', 'NMAXOC', nbsy)
        do 21, j=2,nbsy
        call jelira(jexnum(nomob//'.TACH', j), 'LONMAX', nbch)
        call jeveuo(jexnum(nomob//'.TACH', j), 'L', iatach)
        do 31, i=1,nbch
        nomch=zk24(iatach-1+i)(1:19)
        if (nomch(1:1) .ne. ' ') then
            call jeexin(nomch//'.CELD', iexi)
            if (iexi .gt. 0) then
                repk='OUI'
                goto 9999
            endif
        endif
31      continue
21      continue
        repk='NON'
!
!
!
        else if ( (questi.eq.'NB_CHAMP_MAX') .or. (&
    questi.eq.'NB_CHAMP_UTI')) then
!     ------------------------------------------
        call jelira(nomob//'.DESC', 'GENR', cval=k8bid)
        if (k8bid(1:1) .eq. 'N') then
            call dismrc(questi, nomob, repi, repk, ierd)
        else
            repi = 1
        endif
!
!
    else if (questi.eq.'NB_MODES_TOT') then
!     ------------------------------------------
        call rsorac(nomob, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, k8bid, nbmod, 1,&
                    ibid)
        repi = nbmod
        repk='NON'
!
    else if (questi.eq.'NB_MODES_STA') then
!     ------------------------------------------
        nbstat=0
        call rsorac(nomob, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, k8bid, nbmod, 1,&
                    ibid)
!
        do 41, i=1,nbmod
        call rsadpa(nomob, 'L', 1, 'TYPE_MODE', i,&
                    0, iad, k8bid)
        nomch = zk16(iad)(1:16)
        if (nomch(1:8) .eq. 'MODE_STA') then
            nbstat=nbstat+1
        endif
41      continue
        repi = nbstat
        repk='NON'
!
    else if (questi.eq.'NB_MODES_DYN') then
!     ------------------------------------------
        nbdyn=0
        call rsorac(nomob, 'LONUTI', ibid, rbid, k8bid,&
                    cbid, rbid, k8bid, nbmod, 1,&
                    ibid)
!
        do 51, i=1,nbmod
        call rsadpa(nomob, 'L', 1, 'TYPE_MODE', i,&
                    0, iad, k8bid)
        nomch = zk16(iad)(1:16)
        if ((nomch(1:9).eq.'MODE_DYN')) then
            nbdyn=nbdyn+1
        endif
51      continue
        repi = nbdyn
        repk='NON'
!
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
