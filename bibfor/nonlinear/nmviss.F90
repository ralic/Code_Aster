subroutine nmviss(numedd, sddyna, ds_inout, instam, instap,&
                  vecasz)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/irmit2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynkk.h"
#include "asterfort/r8inir.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rs_getnume.h"
#include "asterfort/rs_getfirst.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dsymv.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=19) :: sddyna
    character(len=24) :: numedd
    type(NL_DS_InOut), intent(in) :: ds_inout
    real(kind=8) :: instam, instap
    character(len=*) :: vecasz
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Computation
!
! Compute FORCE_SOL loads
!
! --------------------------------------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! In  ds_inout         : datastructure for input/output management
! IN  NUMEDD : NUME_DDL
! IN  INSTAM : INSTANT PRECEDENT
! IN  INSTAP : INSTANT COURANT
! OUT VECASS : VECTEUR ASSEMBLEE
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: vecass
    character(len=24) :: chamnd, chand2, chamnv, chanv2, chamna, chana2
    character(len=15) :: sdexso
    character(len=19) :: sdexsz, resu19
    character(len=24) :: tabequ, tabinf
    integer :: ieqint, iddint
    character(len=24) :: tabrig, tabmas, tabamo, tabfor
    integer :: jrigt, jmast, jamot, jfor
    character(len=8) :: k8bid
    integer :: neq
    integer :: nume0, nume
    real(kind=8) :: instd, inst, pas, coef1, coef2
    real(kind=8) :: alpha
    integer :: iordr, iarc, iarc2, iret
    integer :: id1, ifreq
    integer :: jinst, ldnew
    integer :: nddint, unitef, nbmode, npasm, nummax
    character(len=8) :: criterion
    real(kind=8) :: precision
    real(kind=8), pointer :: vaa2(:) => null()
    real(kind=8), pointer :: vad2(:) => null()
    real(kind=8), pointer :: vala(:) => null()
    real(kind=8), pointer :: vald(:) => null()
    real(kind=8), pointer :: valv(:) => null()
    real(kind=8), pointer :: vav2(:) => null()
    real(kind=8), pointer :: trav(:) => null()
    real(kind=8), pointer :: travd(:) => null()
    real(kind=8), pointer :: travv(:) => null()
    real(kind=8), pointer :: trava(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vecass = vecasz
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    tabfor = '&&NMVISS.FORM'
!
! --- ACCES SD EXCIT_SOL
!
    call ndynkk(sddyna, 'SDEXSO', sdexsz)
    sdexso = sdexsz(1:15)
    tabequ = sdexso(1:15)//'.EQINT'
    tabinf = sdexso(1:15)//'.TABI'
    tabrig = sdexso(1:15)//'.RIGT'
    tabmas = sdexso(1:15)//'.MAST'
    tabamo = sdexso(1:15)//'.AMOT'
    call jeveuo(tabequ, 'L', ieqint)
    call jeveuo(tabinf, 'L', iddint)
!
! --- INFORMATIONS GLOBALES
!
    pas    = zr(iddint-1+1)
    unitef = nint(zr(iddint-1+2))
    nddint = nint(zr(iddint-1+3))
    npasm = nint(zr(iddint-1+4))
    nbmode = nddint
!
! - Get parameters
!
    criterion   = ds_inout%criterion
    precision   = ds_inout%precision
    resu19      = ds_inout%result
!
! --- RECUPERATION RESULTATS
!
    inst = int(instam*(1.d0+precision)/pas)*pas
    call jeexin(resu19//'.ORDR', iret)
    if (iret .eq. 0) then
        goto 99
    else
        call rs_getfirst(ds_inout%result, nume0)
        call rs_getnume(ds_inout%result, inst, criterion, precision, nume, iret)
        if (iret.ne.1) then
            call utmess('F', 'DYNAMIQUE_25')
        endif
    endif
!
! --- INITIALISATION DU CHAMP RESULTAT
!
    call jeveuo(vecass(1:19)//'.VALE', 'E', ldnew)
    call r8inir(neq, 0.d0, zr(ldnew), 1)
!
! --- ACCES MATRICES
!
    call jeveuo(tabrig, 'L', jrigt)
    call jeveuo(tabmas, 'L', jmast)
    call jeveuo(tabamo, 'L', jamot)
!
    instd = inst
    coef1 = (instap-instd)/pas
    coef2 = 1.d0-coef1
    nummax = nume+1-nume0
    if (npasm .ne. 0 .and. npasm .lt. (nume+1-nume0)) then
        nummax = npasm
    endif
    write(6,*) 'nummax=',nummax
!
    AS_ALLOCATE(vr=trav, size=nbmode)
    AS_ALLOCATE(vr=travd, size=nbmode)
    AS_ALLOCATE(vr=travv, size=nbmode)
    AS_ALLOCATE(vr=trava, size=nbmode)
    do iordr = 1, nummax
        iarc = iordr+nume0
        iarc2 = nume+1-iordr
        call rsexch('F', ds_inout%result, 'DEPL', iarc2, chamnd,&
                    iret)
        call jeveuo(chamnd(1:19)//'.VALE', 'L', vr=vald)
        call rsexch('F', ds_inout%result, 'VITE', iarc2, chamnv,&
                    iret)
        call jeveuo(chamnv(1:19)//'.VALE', 'L', vr=valv)
        call rsexch('F', ds_inout%result, 'ACCE', iarc2, chamna,&
                    iret)
        call jeveuo(chamna(1:19)//'.VALE', 'L', vr=vala)
        if (iarc2 .gt. 0) then
            call rsexch('F', ds_inout%result, 'DEPL', iarc2-1, chand2,&
                        iret)
            call jeveuo(chand2(1:19)//'.VALE', 'L', vr=vad2)
            call rsexch('F', ds_inout%result, 'VITE', iarc2-1, chanv2,&
                        iret)
            call jeveuo(chanv2(1:19)//'.VALE', 'L', vr=vav2)
            call rsexch('F', ds_inout%result, 'ACCE', iarc2-1, chana2,&
                        iret)
            call jeveuo(chana2(1:19)//'.VALE', 'L', vr=vaa2)
        endif
        if (iordr .eq. (nume+1-nume0)) then
            inst=instd+pas
        else
            call rsadpa(ds_inout%result, 'L', 1, 'INST', iarc,&
                        1, sjv=jinst, styp=k8bid)
            inst=zr(jinst)
        endif
!
        ifreq = int(inst*(1.d0+precision)/pas)+1
                
        if (iarc2 .gt. 0) then 
            do id1 = 1,nbmode
                trav(id1) = zr(ldnew+zi(ieqint+id1-1)-1)
                travd(id1) = coef1 * vald(1+zi(ieqint+id1-1)-1)&
                + coef2 * vad2(1+zi(ieqint+id1-1)-1)
                travv(id1) = coef1 * valv(1+zi(ieqint+id1-1)-1)&
                + coef2 * vav2(1+zi(ieqint+id1-1)-1)
                trava(id1) = coef1 * vala(1+zi(ieqint+id1-1)-1)&
                + coef2 *vaa2(1+zi(ieqint+id1-1)-1)
            end do    
            alpha = -0.5d0
            call dsymv('L', nbmode, alpha, zr(jrigt+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travd, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jrigt+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travd, 1, 1.d0, trav,1)
            call dsymv('L', nbmode, alpha, zr(jamot+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travv, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jamot+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travv, 1, 1.d0, trav,1)
            call dsymv('L', nbmode, alpha, zr(jmast+(ifreq-1)*nbmode*nbmode),&
                        nbmode, trava, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jmast+(ifreq-1)*nbmode*nbmode),&
                        nbmode, trava, 1, 1.d0, trav,1)
        else
            do id1 = 1,nbmode
                trav(id1) = zr(ldnew+zi(ieqint+id1-1)-1)
                travd(id1) = vald(1+zi(ieqint+id1-1)-1)
                travv(id1) = valv(1+zi(ieqint+id1-1)-1)
                trava(id1) = vala(1+zi(ieqint+id1-1)-1)
            end do    
            alpha = -0.5d0 * coef1
            call dsymv('L', nbmode, alpha, zr(jrigt+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travd, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jrigt+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travd, 1, 1.d0, trav,1)
            call dsymv('L', nbmode, alpha, zr(jamot+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travv, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jamot+(ifreq-1)*nbmode*nbmode),&
                        nbmode, travv, 1, 1.d0, trav,1)
            call dsymv('L', nbmode, alpha, zr(jmast+(ifreq-1)*nbmode*nbmode),&
                        nbmode, trava, 1, 1.d0, trav,1)
            call dsymv('U', nbmode, alpha, zr(jmast+(ifreq-1)*nbmode*nbmode),&
                        nbmode, trava, 1, 1.d0, trav,1)
           
        endif
        do id1 = 1,nbmode
            zr(ldnew+zi(ieqint+id1-1)-1) = trav(id1)
        end do    
    end do
    AS_DEALLOCATE(vr=trav)
    AS_DEALLOCATE(vr=travd)
    AS_DEALLOCATE(vr=travv)
    AS_DEALLOCATE(vr=trava)
!
! --- LECTURE FORCES
!
    if (unitef .ne. 0) then
        call wkvect(tabfor, 'V V R', nbmode, jfor)
        call irmit2(nbmode, unitef, instap, tabfor)
        call jeveuo(tabfor, 'L', jfor)
        do id1 = 1, nbmode
            zr(ldnew+zi(ieqint+id1-1)-1)= zr(ldnew+zi(ieqint+id1-1)-1)+zr(jfor+id1-1)
        end do
    endif
!
 99 continue
    call jedetr(tabfor)
    call jedema()
end subroutine
