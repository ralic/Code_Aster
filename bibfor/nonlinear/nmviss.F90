subroutine nmviss(numedd, sddyna, instam, instap, vecasz)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddyna
    character(len=24) :: numedd
    real(kind=8) :: instam, instap
    character(len=*) :: vecasz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES FORCES DE SOL
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMEDD : NUME_DDL
! IN  INSTAM : INSTANT PRECEDENT
! IN  INSTAP : INSTANT COURANT
! OUT VECASS : VECTEUR ASSEMBLEE
!
! ----------------------------------------------------------------------
!
    character(len=8) :: result
    character(len=19) :: vecass
    character(len=24) :: chamnd, chand2, chamnv, chanv2, chamna, chana2
    character(len=15) :: sdexso
    character(len=19) :: sdexsz, resu19
    character(len=24) :: tabequ, tabinf, nomres
    integer :: ieqint, iddint, jnomre
    character(len=24) :: tabrig, tabmas, tabamo, tabfor
    integer :: jrigt, jmast, jamot, jfor
    character(len=8) :: k8bid
    complex(kind=8) :: c16bid
    integer :: neq
    integer :: nume0, nume, nbtro1, nbtro2
    real(kind=8) :: instd, inst, pas, coef1, coef2
    real(kind=8) :: impe12, r8bid
    integer :: iordr, iarc, iarc2, iret, ibid
    integer :: id1, id2, ifreq
    integer :: jinst, ldnew
    integer :: nddint, unitef, nbmode, tnum(1)
    character(len=16) :: motfac
    character(len=8) :: criter
    real(kind=8) :: prec
    real(kind=8), pointer :: vaa2(:) => null()
    real(kind=8), pointer :: vad2(:) => null()
    real(kind=8), pointer :: vala(:) => null()
    real(kind=8), pointer :: vald(:) => null()
    real(kind=8), pointer :: valv(:) => null()
    real(kind=8), pointer :: vav2(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vecass = vecasz
    motfac = 'ETAT_INIT'
    prec = 1.d-6
    criter = 'RELATIF'
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    tabfor = '&&NMVISS.FORM'
!
! --- ACCES SD EXCIT_SOL
!
    call ndynkk(sddyna, 'SDEXSO', sdexsz)
    sdexso = sdexsz(1:15)
    nomres = sdexso(1:15)//'.RESU'
    tabequ = sdexso(1:15)//'.EQINT'
    tabinf = sdexso(1:15)//'.TABI'
    tabrig = sdexso(1:15)//'.RIGT'
    tabmas = sdexso(1:15)//'.MAST'
    tabamo = sdexso(1:15)//'.AMOT'
    call jeveuo(nomres, 'L', jnomre)
    call jeveuo(tabequ, 'L', ieqint)
    call jeveuo(tabinf, 'L', iddint)
!
! --- INFORMATIONS GLOBALES
!
    pas = zr(iddint-1+1)
    unitef = nint(zr(iddint-1+2))
    nddint = nint(zr(iddint-1+3))
    result = zk8(jnomre)
    resu19 = result
    nbmode = nddint
!
! --- TOLERANCES
!
    call getvr8(motfac, 'PRECISION', iocc=1, scal=prec, nbret=iret)
    if (iret .eq. 0) prec = 1.d-6
    call getvtx(motfac, 'CRITERE', iocc=1, scal=criter, nbret=iret)
    if (iret .eq. 0) criter = 'RELATIF'
!
! --- RECUPERATION RESULTATS
!
    inst = int(instam*(1.d0+prec)/pas)*pas
    call jeexin(resu19//'.ORDR', iret)
    if (iret .eq. 0) then
        goto 99
    else
        call rsorac(result, 'PREMIER', ibid, r8bid, k8bid,&
                    c16bid, prec, criter, tnum, 1,&
                    nbtro1)
        nume0=tnum(1)
        call rsorac(result, 'INST', ibid, inst, k8bid,&
                    c16bid, prec, criter, tnum, 1,&
                    nbtro2)
        nume=tnum(1)
        if ((abs(nbtro1).ne.1) .or. (abs(nbtro2).ne.1)) then
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
!
    do iordr = 1, nume+1-nume0
        iarc = iordr+nume0
        iarc2 = nume+1-iordr
        call rsexch('F', result, 'DEPL', iarc2, chamnd,&
                    iret)
        call jeveuo(chamnd(1:19)//'.VALE', 'L', vr=vald)
        call rsexch('F', result, 'VITE', iarc2, chamnv,&
                    iret)
        call jeveuo(chamnv(1:19)//'.VALE', 'L', vr=valv)
        call rsexch('F', result, 'ACCE', iarc2, chamna,&
                    iret)
        call jeveuo(chamna(1:19)//'.VALE', 'L', vr=vala)
        if (iarc2 .gt. 0) then
            call rsexch('F', result, 'DEPL', iarc2-1, chand2,&
                        iret)
            call jeveuo(chand2(1:19)//'.VALE', 'L', vr=vad2)
            call rsexch('F', result, 'VITE', iarc2-1, chanv2,&
                        iret)
            call jeveuo(chanv2(1:19)//'.VALE', 'L', vr=vav2)
            call rsexch('F', result, 'ACCE', iarc2-1, chana2,&
                        iret)
            call jeveuo(chana2(1:19)//'.VALE', 'L', vr=vaa2)
        endif
        if (iordr .eq. (nume+1-nume0)) then
            inst=instd+pas
        else
            call rsadpa(result, 'L', 1, 'INST', iarc,&
                        1, sjv=jinst, styp=k8bid)
            inst=zr(jinst)
        endif
!
        ifreq = int(inst*(1.d0+prec)/pas)+1
        do id1 = 1, nbmode
            do id2 = 1, nbmode
                impe12=0.5d0* (zr(jrigt+(ifreq-1)*nbmode*nbmode+(id2-&
                1)*nbmode+id1-1)+ zr(jrigt+(ifreq-1)*nbmode*nbmode+(&
                id1-1)*nbmode+id2-1))
                zr(ldnew+zi(ieqint+id1-1)-1)= zr(ldnew+zi(ieqint+id1-&
                1)-1)-impe12* vald(1+zi(ieqint+id2-1)-1)*coef1
                if (iarc2 .gt. 0) zr(&
                                  ldnew+zi(ieqint+id1-1)-1)= zr( ldnew+zi(ieqint+id1-1)-1)-impe12&
                                  &* vad2(1+zi(ieqint+ id2-1)-1&
                                  )*coef2
                impe12=0.5d0* (zr(jmast+(ifreq-1)*nbmode*nbmode+(id2-&
                1)*nbmode+id1-1)+ zr(jmast+(ifreq-1)*nbmode*nbmode+(&
                id1-1)*nbmode+id2-1))
                zr(ldnew+zi(ieqint+id1-1)-1)= zr(ldnew+zi(ieqint+id1-&
                1)-1)-impe12* vala(1+zi(ieqint+id2-1)-1)*coef1
                if (iarc2 .gt. 0) zr(&
                                  ldnew+zi(ieqint+id1-1)-1)= zr( ldnew+zi(ieqint+id1-1)-1)-impe12&
                                  &* vaa2(1+zi(ieqint+ id2-1)-1&
                                  )*coef2
                impe12=0.5d0* (zr(jamot+(ifreq-1)*nbmode*nbmode+(id2-&
                1)*nbmode+id1-1)+ zr(jamot+(ifreq-1)*nbmode*nbmode+(&
                id1-1)*nbmode+id2-1))
                zr(ldnew+zi(ieqint+id1-1)-1)= zr(ldnew+zi(ieqint+id1-&
                1)-1)-impe12* valv(1+zi(ieqint+id2-1)-1)*coef1
                if (iarc2 .gt. 0) zr(&
                                  ldnew+zi(ieqint+id1-1)-1)= zr( ldnew+zi(ieqint+id1-1)-1)-impe12&
                                  &* vav2(1+zi(ieqint+ id2-1)-1&
                                  )*coef2
            end do
        end do
    end do
!
! --- LECTURE FORCES
!
    if (unitef .ne. 0) then
        call wkvect(tabfor, 'V V R', nbmode, jfor)
        call irmit2(nbmode, unitef, instap, tabfor)
        call jeveuo(tabfor, 'L', jfor)
        do id1 = 1, nbmode
            zr(ldnew+zi(ieqint+id1-1)-1)= zr(ldnew+zi(ieqint+id1-1)-1)&
            +zr(jfor+id1-1)
        end do
    endif
!
 99 continue
    call jedetr(tabfor)
    call jedema()
end subroutine
