subroutine ascova(detr, vachar, fomulz, npara, vpara,&
                  typres, cnchar)
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
!
!
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/r8depi.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/fointc.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/vtcmbl.h"
#include "asterfort/wkvect.h"
    character(len=*) :: fomulz, npara, typres, cnchar, detr
    character(len=24) :: vachar
    real(kind=8) :: vpara
! ----------------------------------------------------------------------
!  BUT :   AJOUTER / COMBINER DES VECTEURS ASSEMBLES (CHAM_NO)
!
! IN  DETR    : / 'D' : ON DETRUIT  LE VACHAR  (CONSEILLE)
!               / 'G' : ON NE DETRUIT PAS LE VACHAR
! IN/JXVAR  VACHAR  : LISTE DES VECTEURS ASSEMBLES
! IN  FOMULT  : LISTE DES FONCTIONS MULTIPLICATIVES
! IN  NPARA   : NOM DU PARAMETRE
! IN  VPARA   : VALEUR DU PARAMETRE
! IN  TYPRES  : TYPE DES VECTEURS ET DU CHAM_NO RESULTANT 'R' OU 'C'
! VAR/JXOUT  CNCHAR  : CHAM_NO RESULTAT
!
! REMARQUES :
! --------------
! CETTE ROUTINE SERT A COMBINER LES CHAM_NO D'UN "VACHAR"
! UN VACHAR EST UN VECTEUR DE K24 CONTENANT UNE LISTE DE CHAM_NO.
! ON PEUT OBTENIR UN VACHAR PAR ASASVE PAR EXEMPLE.
!
! ATTENTION: SI DETR='D', CETTE ROUTINE DETRUIT LES CHAM_NO DU VACHAR
! =========  APRES LES AVOIR COMBINES. ELLE DETRUIT EGALEMENT LE VACHAR.
!
! POUR TENIR EVENTUELLEMENT COMPTE D'UNE FONC_MULT, ASCOVA
! UTILISE SYSTEMATIQUEMENT LA ROUTINE CORICH SUR LES CHAM_NO
! A COMBINER. IL FAUT QUE LES CHAM_NO DU VACHAR AIENT ETE
! RENSEIGNES PAR CORICH.
!
!
! SI CNCHAR=' ' LE NOM DU CHAMP RESULTAT SERA :
!   CNCHAR=VACHAR(1:8)//'.ASCOVA'
!
!
!
!
    integer :: kk, nbvec, nchar, iret, jvec, jfonct, jcoef, jtype, k
    integer :: icha, ier, n1, npuis, n2, ibid
    real(kind=8) :: valres, valre, valim, dgrd, omega, phase
    logical :: fct
    character(len=19) :: chamno
    character(len=24) :: fomult
    complex(kind=8) :: calpha
    integer :: iarg
!
    call jemarq()
    fomult = fomulz
    dgrd = r8dgrd()
!
!
!     -- ON VERIFIE QUE LE VACHAR A LES BONNES PROPRIETES:
!     ----------------------------------------------------
    call jeexin(vachar, iret)
    ASSERT(iret.ne.0)
    call jelira(vachar, 'LONMAX', nbvec)
    ASSERT(nbvec.ne.0)
    call jeveuo(vachar, 'L', jvec)
!
!
    call jeexin(fomult, iret)
    if (iret .eq. 0) then
        fct = .false.
        nchar = 0
    else
        fct = .true.
        call jelira(fomult, 'LONMAX', nchar)
        ASSERT(nchar.ne.0)
        call jeveuo(fomult, 'L', jfonct)
    endif
!
!
!
!     -- CAS DES CHAM_NO REELS :
!     ----------------------------------------------------
    if (typres(1:1) .eq. 'R') then
        call wkvect('&&ASCOVA.COEF', 'V V R8', nbvec, jcoef)
        call wkvect('&&ASCOVA.TYPE', 'V V K8', nbvec, jtype)
        do 10 k = 1, nbvec
!
            chamno = zk24(jvec+k-1) (1:19)
!         CALL UTIMS2('ASCOVA 1',K,CHAMNO,1,' ')
            call corich('L', chamno, ibid, icha)
!
            ASSERT((icha.ne.0).and.(icha.ge.-2))
!
            if (icha .eq. -1) then
                valres = 1.d0
            else if (icha.eq.-2) then
                valres = 0.d0
            else
                ASSERT(icha.le.nchar)
                valres = 1.d0
                if (fct .and. zk24(jfonct+icha-1) .ne. ' ') then
                    call fointe('F ', zk24(jfonct+icha-1), 1, npara, vpara,&
                                valres, ier)
                else
                    valres=1.d0
                endif
            endif
!
            zr(jcoef+k-1) = valres
            zk8(jtype+k-1) = 'R'
10      continue
!
!
!     -- CAS DES CHAM_NO COMPLEXES :
!     ----------------------------------------------------
    else
        omega = r8depi()*vpara
        kk = 0
        call wkvect('&&ASCOVA.COEF', 'V V R8', 2*nbvec, jcoef)
        call wkvect('&&ASCOVA.TYPE', 'V V K8', nbvec, jtype)
        do 20 k = 1, nbvec
!
            phase = 0.d0
            call getvr8('EXCIT', 'PHAS_DEG', k, iarg, 1,&
                        phase, n1)
            call getvis('EXCIT', 'PUIS_PULS', k, iarg, 1,&
                        npuis, n2)
            calpha = exp(dcmplx(0.d0,phase*dgrd))
            if (npuis .ne. 0) calpha = calpha*omega**npuis
!
            chamno = zk24(jvec+k-1) (1:19)
            call corich('L', chamno, ibid, icha)
!
            ASSERT((icha.ne.0).and.(icha.ge.-2))
!
            if (icha .eq. -1) then
                valre = 1.d0
                valim = 0.d0
            else if (icha.eq.-2) then
                valre = 0.d0
                valim = 0.d0
            else
                ASSERT(icha.le.nchar)
                valre = 1.d0
                valim = 0.d0
                if (fct) call fointc('F', zk24(jfonct+icha-1)(1:8), 1, npara, vpara,&
                                     valre, valim, ier)
            endif
!
            zk8(jtype+k-1) = 'C'
            kk = kk + 1
            zr(jcoef+kk-1) = valre*dble(calpha)-valim*dimag(calpha)
            kk = kk + 1
            zr(jcoef+kk-1) = valim*dble(calpha)+valre*dimag(calpha)
20      continue
    endif
!
!
!     COMBINAISON LINEAIRES DES CHAM_NO :
!     -----------------------------------
    if (cnchar .eq. ' ') cnchar = vachar(1:8)//'.ASCOVA'
    call vtcmbl(nbvec, zk8(jtype), zr(jcoef), zk8(jtype), zk24(jvec),&
                zk8(jtype), cnchar)
    call jedetr('&&ASCOVA.COEF')
    call jedetr('&&ASCOVA.TYPE')
!
!
!     DESTRUCTION DU VACHAR :
!     -----------------------------------
    if (detr .eq. 'D') then
        do 30 k = 1, nbvec
            call corich('S', zk24(jvec+k-1) (1:19), ibid, ibid)
            call detrsd('CHAMP_GD', zk24(jvec+k-1))
30      continue
        call jedetr(vachar)
    else if (detr.eq.'G') then
!       -- EN PRINCIPE UTILISE PAR DYNA_LINE_HARM
    else
        ASSERT(.false.)
    endif
!
!
    call jedema()
end subroutine
