subroutine u195tb(chou)
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
!
!     TRAITEMENT DE COMMANDE:   CREA_CHAMP / OPTION: 'EXTR' / TABLE
!
!     " CREATION D'UN CHAMP A PARTIR D'UNE TABLE "
!
    implicit   none
!
!     ------------------------------------------------------------------
! 0.1. ==> ARGUMENT
!
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cescar.h"
#include "asterfort/cescel.h"
#include "asterfort/cnscno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tabchs.h"
#include "asterfort/u2mess.h"
    character(len=*) :: chou
!
! 0.2. ==> COMMUNS
!
!
!      ==> VARIABLES LOCALES
!
    integer :: n1, n2, jnoma, ibid, iret, nncp
    character(len=3) :: prol0
    character(len=8) :: nomgd, ma, mo, k8b
    character(len=16) :: tychlu, option, typchs, typch2
    character(len=19) :: chs, tabin, ligrel
    integer :: iarg
!
    call jemarq()
!
    chs='&&U195TB.CHAMP_S'
!
    call getvid(' ', 'TABLE', 0, iarg, 1,&
                tabin, n1)
    call getvtx(' ', 'TYPE_CHAM', 0, iarg, 1,&
                tychlu, n1)
!
    typchs=tychlu(1:4)
    typch2=typchs
    if (typch2 .eq. 'CART') typch2='ELEM'
    nomgd=tychlu(6:11)
!
!     VERIFICATIONS
    if (typchs .eq. 'NOEU' .or. typchs .eq. 'CART') then
        call getvid(' ', 'MAILLAGE', 0, iarg, 1,&
                    ma, n1)
        if (n1 .eq. 0) call u2mess('F', 'MODELISA7_61')
        option=' '
        mo=' '
    else if (typchs(1:2).eq.'EL') then
        call getvid(' ', 'MODELE', 0, iarg, 0,&
                    k8b, n1)
        call getvtx(' ', 'OPTION', 0, iarg, 0,&
                    k8b, n2)
        if (n1 .ne. 0) then
            n1=-n1
            call getvid(' ', 'MODELE', 0, iarg, 1,&
                        mo, n1)
        endif
        if (n2 .ne. 0) then
            n2=-n2
            call getvtx(' ', 'OPTION', 0, iarg, 1,&
                        option, n2)
        endif
        if (n1 .eq. 0 .or. n2 .eq. 0) call u2mess('F', 'MODELISA7_62')
        call jeveuo(mo//'.MODELE    .LGRF', 'L', jnoma)
        ma=zk8(jnoma)
    endif
!
!     CREATION DU CHAMP SIMPLE
    call tabchs(tabin, typch2, 'V', nomgd, ma,&
                chs)
!
!     TRANSFORMATION : CHAM_S --> CHAMP
    call getvtx(' ', 'PROL_ZERO', 0, iarg, 1,&
                prol0, n1)
    if (n1 .eq. 0) prol0='NON'
    if (typchs .eq. 'NOEU') then
        call cnscno(chs, ' ', prol0, 'G', chou,&
                    'F', ibid)
    else if (typchs(1:2).eq.'EL') then
        call dismoi('F', 'NOM_LIGREL', mo, 'MODELE', ibid,&
                    ligrel, iret)
!        -- POUR L'UTILISATEUR DISTRAIT :
        if (option .eq. 'VARI_ELGA') option='RAPH_MECA'
        call cescel(chs, ligrel, option, ' ', prol0,&
                    nncp, 'G', chou, 'F', ibid)
    else if (typchs.eq.'CART') then
        call cescar(chs, chou, 'G')
    else
        call assert(.false.)
    endif
    call jedema()
!
end subroutine
