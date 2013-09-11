subroutine rcevfa(nommat, para, sm, cnoc, csno,&
                  csne, cspo, cspe, kemixt, cspto,&
                  cspte, cspmo, cspme, cfao, cfae)
    implicit     none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/limend.h"
#include "asterfort/prccm3.h"
#include "asterfort/rcvale.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
    real(kind=8) :: para(3), sm
    character(len=8) :: nommat
    character(len=24) :: cnoc, csno, csne, cspo, cspe, cfao, cfae, cspto, cspte
    character(len=24) :: cspmo, cspme
    logical :: kemixt
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
!     CALCUL DU KE, SALT, NADM ET DOMMAGE
!
!     ------------------------------------------------------------------
!
    integer :: nbordr, jsno, jsne, jspo, jspe, jfao, jfae, ind, jnoc, nbinst, i1
    integer :: i2, jspto, jspte, jspmo, jspme
    real(kind=8) :: sno, sne, spo, spe, keo, kee, salto, salte, nadmo(1), nadme(1)
    real(kind=8) :: kth, ketheo, kethee, spto, spte, spmo, spme, kemeco, kemece
    real(kind=8) :: nbid, saltmo, saltme, saltho, salthe, valr(2)
    character(len=8) :: k8b
    integer :: icodre(1)
    logical :: endur
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call jelira(csno, 'LONMAX', nbordr)
    call jeveuo(csno, 'L', jsno)
    call jeveuo(csne, 'L', jsne)
    call jeveuo(cspo, 'L', jspo)
    call jeveuo(cspe, 'L', jspe)
    if (kemixt) then
        call jeveuo(cspto, 'L', jspto)
        call jeveuo(cspte, 'L', jspte)
        call jeveuo(cspmo, 'L', jspmo)
        call jeveuo(cspme, 'L', jspme)
    endif
    call jelira(cnoc, 'LONMAX', nbinst)
    call jeveuo(cnoc, 'L', jnoc)
!
    call wkvect(cfao, 'V V R', 5*nbordr, jfao)
    call wkvect(cfae, 'V V R', 5*nbordr, jfae)
!
    ind = 0
!
! ---     POUR TOUTES COMBINAISONS D INSTANTS : CALCUL AUX DEUX
! ---     EXTREMITES :
! ---     - DU COEFFICIENT DE CONCENTRATION ELASTO-PLASTIQUE KE
! ---     - DE LA CONTRAINTE EQUIVALENTE ALTERNEE SALT
! ---     - DU NBRE DE CYCLES ADMISSIBLE NADM (AVEC LA COURBE DE WOHLER)
! ---     - DU FACTEUR D USAGE
!         --------------------------------------------------------
    do 10 i1 = 1, nbinst
!
        ind = ind + 1
        zr(jfao-1+5*(ind-1)+4) = 0.d0
        zr(jfae-1+5*(ind-1)+4) = 0.d0
!
        do 12 i2 = i1+1, nbinst
!
            ind = ind + 1
            sno = zr(jsno+ind-1)
            sne = zr(jsne+ind-1)
!
            if (.not. kemixt) then
!
! --- 1ER CAS : KE_MECA
!
                spo = zr(jspo+ind-1)
                spe = zr(jspe+ind-1)
!
                call prccm3(nommat, para, sm, sno, spo,&
                            keo, salto, nadmo(1))
                call prccm3(nommat, para, sm, sne, spe,&
                            kee, salte, nadme(1))
!
                zr(jfao-1+5*(ind-1)+1) = keo
                zr(jfao-1+5*(ind-1)+2) = salto
                zr(jfao-1+5*(ind-1)+3) = nadmo(1)
!
                zr(jfae-1+5*(ind-1)+1) = kee
                zr(jfae-1+5*(ind-1)+2) = salte
                zr(jfae-1+5*(ind-1)+3) = nadme(1)
!
                zr(jfao-1+5*(ind-1)+4) = 1.d0 / nadmo(1)
                zr(jfae-1+5*(ind-1)+4) = 1.d0 / nadme(1)
            else
!
! --- 2EME CAS : KE_MIXTE
!
                spmo = zr(jspmo+ind-1)
                spme = zr(jspme+ind-1)
                spto = zr(jspto+ind-1)
                spte = zr(jspte+ind-1)
!
                kth = 1.86d0*(1.d0-(1.d0/(1.66d0+sno/sm)))
                ketheo = max(1.d0,kth)
                saltho = 0.5d0 * para(3) * ketheo * spto
!
                kth = 1.86d0*(1.d0-(1.d0/(1.66d0+sne/sm)))
                kethee = max(1.d0,kth)
                salthe = 0.5d0 * para(3) * kethee * spte
!
                call prccm3(nommat, para, sm, sno, spmo,&
                            kemeco, saltmo, nbid)
                call prccm3(nommat, para, sm, sne, spme,&
                            kemece, saltme, nbid)
                salto = saltmo + saltho
                salte = saltme + salthe
!
! --- CALCUL DU NOMBRE DE CYCLES ADMISSIBLE NADM
!
                call limend(nommat, salto, 'WOHLER', k8b, endur)
                if (endur) then
                    nadmo(1)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', [salto],&
                                1, 'WOHLER  ', nadmo(1), icodre(1), 2)
                    if (nadmo(1) .lt. 0) then
                        valr (1) = salto
                        valr (2) = nadmo(1)
                        call u2mesg('A', 'POSTRELE_61', 0, ' ', 0,&
                                    0, 2, valr)
                    endif
                endif
!
                call limend(nommat, salte, 'WOHLER', k8b, endur)
                if (endur) then
                    nadme(1)=r8maem()
                else
                    call rcvale(nommat, 'FATIGUE', 1, 'SIGM    ', [salte],&
                                1, 'WOHLER  ', nadme(1), icodre(1), 2)
                    if (nadmo(1) .lt. 0) then
                        valr (1) = salte
                        valr (2) = nadme(1)
                        call u2mesg('A', 'POSTRELE_61', 0, ' ', 0,&
                                    0, 2, valr)
                    endif
                endif
!
                zr(jfao-1+5*(ind-1)+1) = kemeco
                zr(jfao-1+5*(ind-1)+5) = ketheo
                zr(jfao-1+5*(ind-1)+2) = salto
                zr(jfao-1+5*(ind-1)+3) = nadmo(1)
                zr(jfao-1+5*(ind-1)+4) = 1.d0 / nadmo(1)
!
                zr(jfae-1+5*(ind-1)+1) = kemece
                zr(jfae-1+5*(ind-1)+5) = kethee
                zr(jfae-1+5*(ind-1)+2) = salte
                zr(jfae-1+5*(ind-1)+3) = nadme(1)
                zr(jfae-1+5*(ind-1)+4) = 1.d0 / nadme(1)
!
            endif
!
12      continue
!
10  end do
!
    call jedema()
end subroutine
