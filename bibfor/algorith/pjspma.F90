subroutine pjspma(corres, cham1, cham2, prol0, ligre2,&
                  noca, base, iret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!                   Commande PROJ_CHAMP
!
!  Routine "chapeau" : projection aux sous-points
!
!    - appel a pjxxch (usuel pour tous les champs, cf. op0166 ou pjxxpr)
!    - appel a pjcor2 (retour aux sous-points)
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=1) :: base
    character(len=8) :: prol0, noca
    character(len=16) :: corres
    character(len=19) :: cham1, cham2, ligre2
    integer :: iret
!
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/pjcor2.h"
#include "asterfort/pjxxch.h"
#include "asterfort/titre.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nncp, ierd
    character(len= 4) :: tycha2
    character(len= 8) :: nompar
    character(len=16) :: option
    character(len=19) :: chauxs, prfchn, cns1, ch2s
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    call titre()
!   projection du champ sur le maillage masp
    chauxs = '&&PJSPMA'//'.CHAS'
    tycha2 = 'NOEU'
    call pjxxch(corres, cham1, chauxs, tycha2, ' ',&
                prol0, ligre2, base, iret)
    if (iret .ne. 0) goto 999
!
!   chauxs : valeur aux noeud du maillage masp
!   on transforme le CHAM_NO projeté en un CHAM_NO_S
    cns1 = '&&PJSPMA'//'.CH1S'
    call cnocns(chauxs, 'G', cns1)
!
!   cns1 : CHAM_NO_S des valeurs aux noeuds de masp
!   on transpose le champ sur le modèle 2 (ELGA sous-points)
    ch2s = '&&OP0166'//'.CH2S'
    call pjcor2(noca, cns1, ch2s, ligre2, corres,&
                nompar, option, ierd)
!
    ASSERT( (option.eq.'INI_SP_MATER').or.(option.eq.'INI_SP_RIGI') )
!
    call cescel(ch2s, ligre2, option, nompar, prol0,&
                nncp, 'G', cham2, 'A', ierd)
!
    call dismoi('PROF_CHNO', chauxs, 'CHAM_NO', repk=prfchn)
    call detrsd('PROF_CHNO', prfchn)
!
    call detrsd('CHAM_NO_S', cns1)
    call detrsd('CHAM_ELEM_S', ch2s)
!
999 continue
    call detrsd('CHAM_NO', chauxs)
    call jedema()
end subroutine
