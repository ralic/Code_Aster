subroutine rcveri(tablz)
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lcprsn.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/tbexip.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbextb.h"
#include "asterfort/tbexv1.h"
#include "asterfort/tbexve.h"
#include "asterfort/utmess.h"
    character(len=*) :: tablz
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
!     ------------------------------------------------------------------
!      OPERATEUR POST_RCCM
!      VERIFICATION
!      - SUR L'ORDRE DES NOEUDS DANS LA TABLE
!      - SUR L'ALIGNEMENT DES NOEUDS DANS LA TABLE
!
!     IN  TABLE: TABLE A EXPLOITER
!
!     ------------------------------------------------------------------
    integer :: jinst, nbno, jcox, jcoy, jcoz, ibid, i, nbinst, iret, nbinti
    integer :: inti, jinti, n1
    real(kind=8) :: ab(3), am(3), ps, eps, nor(3), norme, inst, r8b, eps2
    real(kind=8) :: maxdis
    real(kind=8) :: mm(3), noram, nornor, norab, valr(2), disrel
    complex(kind=8) :: cbid
    character(len=8) :: k8b, crit, tyva
    character(len=16) :: valek(5), table, tbtmp1, tbtmp2, titu, valk(2), typmec
    character(len=24) :: coorx, coory, coorz, instan, intitu
    logical :: exi1, exi2, exi3, exist, noinst, impnm
    parameter(crit='RELATIF ',eps=1.0d-6, eps2=1.0d-2)
!
    call jemarq()
!
    valek(1) = 'INST            '
    valek(2) = 'COOR_X          '
    valek(3) = 'COOR_Y          '
    valek(4) = 'COOR_Z          '
    valek(5) = 'INTITULE        '
    tbtmp1 = '&&RCVERI_TBTMP1 '
    tbtmp2 = '&&RCVERI_TBTMP2 '
    coorx = '&&RCVERI_COOR_X '
    coory = '&&RCVERI_COOR_Y '
    coorz = '&&RCVERI_COOR_Z '
    instan = '&&RCVERI_INST   '
    intitu = '&&RCVERI_INTITU '
    table = tablz
    noinst = .false.
    impnm = .false.
!
!     VERIFICATION DE LA PRESENCE DU PARAMETRE "INST"
    call tbexip(table, valek(1), exist, k8b)
    if (.not.exist) then
!       LA VERIFICATION PORTERA SUR TOUS LES NOEUDS
!       CONTENUS DANS LA TABLE
        nbinti=1
        noinst=.true.
    else
!        VERIFICATION DE LA PRESENCE DU PARAMETRE "INTITULE"
        call tbexp2(table, valek(5))
!        RECUPERATION DES INTITULES DE LA TABLE
        call tbexv1(table, valek(5), intitu, 'V', nbinti,&
                    tyva)
        call jeveuo(intitu, 'L', jinti)
    endif
!
!     VERIFICATION DU NOMBRE DE LIGAMENT DANS LE CAS UNITAIRE
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
    if (typmec .eq. 'UNITAIRE' .and. nbinti .ne. 1) then
        call utmess('F', 'POSTRCCM_40', sk=table, si=nbinti)
    endif
!
!     VERIFICATION DE LA PRESENCE DES COORDONNEES DANS LA TABLE
    call tbexip(table, valek(2), exi1, k8b)
    call tbexip(table, valek(3), exi2, k8b)
    call tbexip(table, valek(4), exi3, k8b)
    if (.not.exi1 .and. .not.exi2 .and. .not.exi3) then
        call utmess('I', 'POSTRCCM_39', sk=table)
        goto 999
    endif
!
! ------------------------
!     ON PARCOURT LES INTITULES
    do 100 inti = 1, nbinti
!
        if (noinst) then
!
            call copisd('TABLE', 'V', table, tbtmp2)
!
        else
!
!         EXTRACTION D'UNE NOUVELLE TABLE A PARTIR DE "TABLE"
!         CONTENANT UNIQUEMENT L'INTITULE EN COURS
            if (tyva(1:2) .eq. 'K8') then
                call tbextb(table, 'V', tbtmp1, 1, valek(5),&
                            'EQ', [ibid], [r8b], [cbid], zk8(jinti+inti-1),&
                            [r8b], k8b, iret)
            else if (tyva(1:3).eq.'K16') then
                call tbextb(table, 'V', tbtmp1, 1, valek(5),&
                            'EQ', [ibid], [r8b], [cbid], zk16(jinti+inti-1),&
                            [r8b], k8b, iret)
            else
                call utmess('F', 'DVP_1')
            endif
!
!         LECTURE DU PREMIER INSTANT
            call tbexv1(tbtmp1, valek(1), instan, 'V', nbinst,&
                        k8b)
            call jeveuo(instan, 'L', jinst)
            inst=zr(jinst)
!         EXTRACTION D'UNE NOUVELLE TABLE A PARTIR DE "TBTMP1"
!         CONTENANT UNIQUEMENT L'INSTANT "INST".
            call tbextb(tbtmp1, 'V', tbtmp2, 1, valek(1),&
                        'EQ', [ibid], [inst], [cbid], k8b,&
                        [eps], crit, iret)
            call detrsd('TABLE', tbtmp1)
            call jedetr(instan)
!
        endif
!
!       LECTURE DE LA COLONNE "COOR_X"
        call tbexve(tbtmp2, valek(2), coorx, 'V', nbno,&
                    k8b)
        call jeveuo(coorx, 'L', jcox)
!       LECTURE DE LA COLONNE "COOR_Y"
        call tbexve(tbtmp2, valek(3), coory, 'V', nbno,&
                    k8b)
        call jeveuo(coory, 'L', jcoy)
!       LECTURE DE LA COLONNE "COOR_Z"
        call tbexve(tbtmp2, valek(4), coorz, 'V', nbno,&
                    k8b)
        call jeveuo(coorz, 'L', jcoz)
        call detrsd('TABLE', tbtmp2)
!
        ab(1)=zr(jcox+nbno-1)-zr(jcox)
        ab(2)=zr(jcoy+nbno-1)-zr(jcoy)
        ab(3)=zr(jcoz+nbno-1)-zr(jcoz)
        call normev(ab, norab)
!
!       ON VERIFIE QUE LES NOEUDS SONT ORDONNES
        do 10 i = 1, nbno-1
            mm(1)=zr(jcox+i)-zr(jcox+i-1)
            mm(2)=zr(jcoy+i)-zr(jcoy+i-1)
            mm(3)=zr(jcoz+i)-zr(jcoz+i-1)
            call normev(mm, norme)
            call lcprsn(3, mm, ab, ps)
            if (ps .le. eps) then
                call utmess('F', 'POSTRCCM_37', sk=table)
            endif
10      continue
!
!       ON VERIFIE QUE LES NOEUDS SONT ALIGNES
        maxdis=eps2
        do 20 i = 1, nbno-1
            am(1)=zr(jcox+i)-zr(jcox)
            am(2)=zr(jcoy+i)-zr(jcoy)
            am(3)=zr(jcoz+i)-zr(jcoz)
            call normev(am, noram)
            call provec(am, ab, nor)
            call normev(nor, nornor)
            disrel=noram*nornor/norab
            if (disrel .gt. eps2) then
                impnm=.true.
                maxdis=max(disrel,maxdis)
            endif
20      continue
!
        if (impnm) then
            if (tyva(1:2) .eq. 'K8') then
                titu= zk8(jinti+inti-1)
            else if (tyva(1:3).eq.'K16') then
                titu=zk16(jinti+inti-1)
            endif
            valk(1)=table
            valk(2)=titu(1:len(titu))
            valr(1)=maxdis*norab
            valr(2)=norab
            call utmess('A', 'POSTRCCM_38', nk=2, valk=valk, nr=2,&
                        valr=valr)
        endif
!
        call jedetr(coorx)
        call jedetr(coory)
        call jedetr(coorz)
!
!
100  end do
!
!
!
! ------------------------
!
999  continue
!
    call jedetr(intitu)
!
    call jedema()
!
end subroutine
