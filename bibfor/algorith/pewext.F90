subroutine pewext(resu)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsdot.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/u2mesk.h"
!
    character(len=*) :: resu
! ----------------------------------------------------------------------
!
    integer :: i, iret, jinst
    integer :: nbord, jord, numord
    real(kind=8) :: inst, prec, f0u0, f1u0, f0u1, f1u1, w, valer(3)
    complex(kind=8) :: c16b
    character(len=8) :: crit, result
    character(len=8) :: k8b, typarr(4)
    character(len=16) :: noparr(4)
    character(len=19) :: depla1, force1
    character(len=19) :: depls0, depls1, forcs0, forcs1
    character(len=24) :: lisord
    integer ::  ier
!
!-----------------------------------------------------------------------
!
!
    call jemarq()
    lisord='&&PEWEXT.VECTORDR'
    call getvid(' ', 'RESULTAT', scal=result, nbret=iret)
!
!
! -- INITIALISATION DE LA TABLE RESULTAT
!
    typarr(1)='I'
    typarr(2)='R'
    typarr(3)='R'
    typarr(4)='R'
!
    noparr(1)='NUME_ORDRE'
    noparr(2)='INST'
    noparr(3)='TRAV_ELAS'
    noparr(4)='TRAV_REEL'
!
    call tbcrsd(resu, 'G')
    call tbajpa(resu, 4, noparr, typarr)
!
!
!
! -- EXTRACTION DES NUMEROS D'ORDRE DU CALCUL
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=iret)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=iret)
    call rsutnu(result, ' ', 0, lisord, nbord,&
                prec, crit, iret)
    if (iret .ne. 0) call u2mesk('F', 'POSTELEM_11', 1, result)
    call jeveuo(lisord, 'L', jord)
!
!
! -- CALCUL DU TRAVAIL DES FORCES EXTERIEURES AUX DIFFERENTS INSTANTS
!
    depls0='&&PEWEXT.DEPLS0'
    depls1='&&PEWEXT.DEPLS1'
    forcs0='&&PEWEXT.FORCS0'
    forcs1='&&PEWEXT.FORCS1'
!
    do 10 i = 1, nbord
        call jemarq()
        call jerecu('V')
        numord=zi(jord-1+i)
!
!       EXTRACTION DE L'INSTANT DE CALCUL
        call rsadpa(result, 'L', 1, 'INST', numord,&
                    0, jinst, k8b)
        inst=zr(jinst)
!
!       EXTRACTION DU CHAMP DE DEPLCAMENT
        call rsexch('F', result, 'DEPL', numord, depla1,&
                    iret)
!
!       -- TOUS LES CHAMPS DE LA SD_RESULTAT N'ONT PAS FORCEMENT
!          LA MEME NUMEROTATION, C'EST POURQUOI ON PASSE PAR DES
!          CHAMPS SIMPLES :
        call cnocns(depla1, 'V', depls1)
!
!       EXTRACTION DU CHAMP DE FORCE NODALE
        call rsexch('F', result, 'FORC_NODA', numord, force1,&
                    iret)
        call cnocns(force1, 'V', forcs1)
!
!       CALCUL DU PRODUIT SCALAIRE F.U
        call cnsdot(depls1, forcs1, f1u1, ier)
        ASSERT(ier.eq.0)
!
!       CALCUL DE L'INTEGRALE I(F.DU)
        if (i .ge. 2) then
            call cnsdot(depls0, forcs1, f1u0, ier)
            ASSERT(ier.eq.0)
            call cnsdot(depls1, forcs0, f0u1, ier)
            ASSERT(ier.eq.0)
            w=w+0.5d0*(f0u1-f1u0+f1u1-f0u0)
        else
            w=0
        endif
!
        valer(1)=inst
        valer(2)=f1u1/2
        valer(3)=w
        call tbajli(resu, 4, noparr, numord, valer,&
                    c16b, k8b, 0)
!
        call copisd('CHAM_NO_S', 'V', depls1, depls0)
        call copisd('CHAM_NO_S', 'V', forcs1, forcs0)
        f0u0=f1u1
!
        call jedema()
10  end do
!
    call detrsd('CHAM_NO_S', depls1)
    call detrsd('CHAM_NO_S', depls0)
    call detrsd('CHAM_NO_S', forcs1)
    call detrsd('CHAM_NO_S', forcs0)
!
    call jedema()
end subroutine
