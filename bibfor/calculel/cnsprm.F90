subroutine cnsprm(cns1z, basez, cns2z, iret)
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
#include "asterc/r8prem.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
!
    integer :: iret
    character(len=*) :: cns1z, basez, cns2z
! ------------------------------------------------------------------
! BUT : PROJETER UN CHAM_NO_S  SUR UN MAILLAGE MESURE
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNS1Z  IN/JXIN  K19 : CHAM_NO_S A PROJETER
! BASEZ  IN       K1  : BASE DE CREATION POUR CNS2Z : G/V/L
! CNS2Z  IN/JXOUT K19 : CHAM_NO_S RESULTAT DE LA PROJECTION
! IRET   OUT      I   : IRET = 0 : OK / IRET = 1 : PB
! ------------------------------------------------------------------
!    ON NE TRAITE QUE LES CHAMPS REELS (R8) OU COMPLEXES (C16)
!
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    logical :: newk, axe
    character(len=1) :: base
    character(len=3) :: tsca, dir
    character(len=8) :: ma1, ma2, ma3, nomgd, promes, macrel
    character(len=8) :: model3, kcmp, nono, kcmp2
    character(len=8) :: basemo, mail, numddl, licmp, ksto
    character(len=16) :: k16bid
    character(len=19) :: cns1, cns2, trav
    character(len=24) :: vnoeud, vrange, vnoeum, vrangm, vmes, vsu, k24bid
    character(len=24) :: valk(2), vorien, vref, vrefpm
    integer :: jcns1c, jcns1l, jcns1v, jcns1k, jcns1d, icmp1, icmp2
    integer :: jcns2c, jcns2l, jcns2v, jcns2k, jcns2d, lvsu, lcmp
    integer :: ncmp, ibid, gd, ncmp2, ino2, icmp, ino1, icmpd
    integer :: iamacr, isma, lori, lref, lrefms
    integer :: iddl, jddl, imod, ipos, iposi, iposj, lnoeud, lrange
    integer :: lnoeum, lrangm, nbmesu, nbord, nddle, lmesu, ltrav
    real(kind=8) :: v1, v2, coef1, valx, valy, valz, eps
    complex(kind=8) :: v1c, v2c
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    cns1 = cns1z
    cns2 = cns2z
    base = basez
!
! RECUPERATION DES OBJETS ET INFORMATIONS DE CNS1 :
!
    call jeveuo(cns1//'.CNSK', 'L', jcns1k)
    call jeveuo(cns1//'.CNSD', 'L', jcns1d)
    call jeveuo(cns1//'.CNSC', 'L', jcns1c)
    call jeveuo(cns1//'.CNSV', 'L', jcns1v)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
!
! MA1 : MAILLAGE DE LA MODIFICATION
    ma1 = zk8(jcns1k-1+1)
    nomgd = zk8(jcns1k-1+2)
    ncmp = zi(jcns1d-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
    call jeveuo(ma1//'.NOMACR', 'L', iamacr)
!
    call getvtx(' ', 'SUPER_MAILLE', scal=mail, nbret=ibid)
!
    call jenonu(jexnom(ma1//'.SUPMAIL', mail), isma)
    if (isma .le. 0) then
        valk(1)=mail
        valk(2)=ma1
        call u2mesk('F', 'CALCULEL5_53', 2, valk)
    endif
    macrel= zk8(iamacr-1+isma)
!
    call dismoi('F', 'NOM_PROJ_MESU', macrel, 'MACR_ELEM_STAT', ibid,&
                promes, ibid)
!
! RECUPERATION DES ELEMENTS RELATIFS A LA MESURE
    vnoeud = promes//'.PROJM    .PJMNO'
    vrange = promes//'.PROJM    .PJMRG'
    vorien = promes//'.PROJM    .PJMOR'
    vrefpm = promes//'.PROJM    .PJMRF'
!
    call jeveuo(vnoeud, 'L', lnoeud)
    call jelira(vnoeud, 'LONUTI', nbmesu)
!
! MODEL3 : MODELE MESURE
    call jeveuo(vrange, 'L', lrange)
    call jeveuo(vrefpm, 'L', lrefms)
    k16bid=zk16(lrefms-1 +1)
    model3=k16bid(1:8)
!
    vref = macrel//'.PROJM    .PJMRF'
    call jeveuo(vref, 'L', lref)
    k16bid=zk16(lref-1 +3)
    basemo=k16bid(1:8)
!
! POUR LES ORIENTATIONS DES CAPTEURS
    call jeveuo(vorien, 'L', lori)
!
! BASEMO : POUR LA RECUPERATION DU MAILLAGE DU MODELE SUPPORT (MA2)
!
    call dismoi('F', 'NUME_DDL', basemo, 'RESU_DYNA', ibid,&
                k24bid, iret)
    numddl = k24bid(1:8)
    call dismoi('F', 'NOM_MAILLA', numddl, 'NUME_DDL', ibid,&
                ma2, ibid)
!
    call dismoi('F', 'NOM_MAILLA', model3, 'MODELE', ibid,&
                ma3, ibid)
!
!  QUELQUES VERIFS :
    if (tsca .ne. 'R' .and. tsca .ne. 'C') then
!        -- ON NE TRAITE QUE LES CHAMPS R/C :
        iret = 1
        goto 9999
    endif
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) call u2mesk('F', 'CALCULEL_67', 1, nomgd)
!
! ALLOCATION DE CNS2 :
    call detrsd('CHAM_NO_S', cns2)
!
! FAIRE APPEL A VRANGE POUR LA LISTE DES CMP MESURE
! ON FAIT L UNION DES CMP DE CNS1 ET VRANGE
!
    licmp = '&&LICMP'
    call wkvect(licmp, 'V V K8', 3*ncmp, lcmp)
    do 230 icmp = 1, ncmp
        zk8(lcmp-1+icmp)=zk8(jcns1c-1+icmp)
230  continue
    ncmp2 = ncmp
    do 220 iddl = 1, nbmesu
        kcmp = zk8(lrange-1+iddl)
        newk = .true.
        do 210 icmp = 1, ncmp2
            ksto = zk8(lcmp-1+icmp)
            if (kcmp .eq. ksto) newk = .false.
210      continue
        if (newk) then
            ncmp2 = ncmp2+1
            zk8(lcmp-1+ncmp2) = kcmp
        endif
220  continue
!
    call cnscre(ma3, nomgd, ncmp2, zk8(lcmp), base,&
                cns2)
    call jeveuo(cns2//'.CNSK', 'L', jcns2k)
    call jeveuo(cns2//'.CNSD', 'L', jcns2d)
    call jeveuo(cns2//'.CNSC', 'L', jcns2c)
    call jeveuo(cns2//'.CNSV', 'E', jcns2v)
    call jeveuo(cns2//'.CNSL', 'E', jcns2l)
!
! LISTE DES NOEUDS DU MACRO ELEMENT
    vnoeum = macrel//'.PROJM    .PJMNO'
    vrangm = macrel//'.PROJM    .PJMRG'
    call jeveuo(vnoeum, 'L', lnoeum)
    call jeveuo(vrangm, 'L', lrangm)
    call jelira(vnoeum, 'LONUTI', nddle)
!
! INVERSE DE LA MATRICE DE PASSAGE : VSU = (TIT*PHI)-1
    vsu = macrel//'.PROJM    .PJMIG'
    call jeveuo(vsu, 'L', lvsu)
    call jelira(vsu, 'LONUTI', nbord)
    nbord = nbord/nddle
! NBORD : NOMBRE DE NUMERO D'ORDRE (MODE MESURE)
!
! RECUPERATION DES MODES MESURES
    vmes = macrel//'.PROJM    .PJMMM'
    call jeveuo(vmes, 'L', lmesu)
!
    trav = '&TRAV'
    call wkvect(trav, 'V V R', nbmesu*nddle, ltrav)
! CALCUL DU PRODUIT : PHI*VSU
    do 410 iddl = 1, nbmesu
        do 420 jddl = 1, nddle
            ipos = (jddl-1)*nbmesu+iddl
            zr(ltrav-1+ipos) = 0.d0
            do 430 imod = 1, nbord
                iposi = (imod-1)*nbmesu+iddl
                iposj = (jddl-1)*nbord+imod
                zr(ltrav-1+ipos) = zr(ltrav-1+ipos) + zr(lmesu-1+ iposi)*zr(lvsu-1+iposj)
430          continue
420      continue
410  continue
!
!
! INITIALISATION A ZERO
    v2=0.d0
    v2c = dcmplx(0.d0,0.d0)
!
    do 110 iddl = 1, nbmesu
        ino2 = zi(lnoeud-1+iddl)
        do 70 icmp = 1, ncmp2
            zl(jcns2l-1+ (ino2-1)*ncmp2+icmp)=.true.
            if (tsca .eq. 'R') then
                zr(jcns2v-1+ (ino2-1)*ncmp2+icmp)=v2
            else
                zc(jcns2v-1+ (ino2-1)*ncmp2+icmp)=v2c
            endif
70      continue
110  continue
!
!
! PROJECTION DU CHAMP SUIVANT LA DIRECTION DE MESURE
!
    do 100 iddl = 1, nbmesu
        ino2 = zi(lnoeud-1+iddl)
        kcmp2 = zk8(lrange-1+iddl)
!
        do 50 icmp = 1, ncmp2
            if (zk8(jcns2c-1+icmp) .eq. kcmp2) then
                icmp2=icmp
                goto 60
            endif
50      continue
60      continue
!
        v2=0.d0
        v2c = dcmplx(0.d0,0.d0)
!
        do 200 jddl = 1, nddle
            ino1 = zi(lnoeum-1+jddl)
            kcmp = zk8(lrangm-1+jddl)
! ICI ON SUPPOSE QUE LES NOEUDS INTERFACES ONT LE MEME NOM
            call jenuno(jexnum(ma2//'.NOMNOE', ino1), nono)
            call jenonu(jexnom(ma1//'.NOMNOE', nono), ino1)
!
            do 150 icmp = 1, ncmp
                if (zk8(jcns1c-1+icmp) .eq. kcmp) then
                    icmp1=icmp
                    goto 160
                endif
150          continue
160          continue
!
            coef1 = zr(ltrav-1+(jddl-1)*nbmesu+iddl)
!
            if (tsca .eq. 'R') then
                v1=zr(jcns1v-1+(ino1-1)*ncmp+icmp1)
                v2=v2+coef1*v1
            else
                v1c=zc(jcns1v-1+(ino1-1)*ncmp+icmp1)
                v2c=v2c+coef1*v1c
            endif
!
200      continue
!
        zl(jcns2l-1+ (ino2-1)*ncmp2+icmp2)=.true.
        if (tsca .eq. 'R') then
            zr(jcns2v-1+ (ino2-1)*ncmp2+icmp2)=v2
        else
            zc(jcns2v-1+ (ino2-1)*ncmp2+icmp2)=v2c
        endif
!
! VERIFICATION SI LA MESURE EST SUR UN DES AXES DE COORDONNEES
! CERTAINS UTILISATEURS SONT HABITUES AUX CMP DX, DY, DZ
        if ((kcmp2.eq.'D1') .or. (kcmp2.eq.'D2') .or. (kcmp2.eq.'D3')) then
            valx = zr(lori-1+(iddl-1)*3+1)
            valy = zr(lori-1+(iddl-1)*3+2)
            valz = zr(lori-1+(iddl-1)*3+3)
!
            valx= abs(valx)
            valy= abs(valy)
            valz= abs(valz)
!
            eps = 1.d2*r8prem()
            axe = .false.
            if ((valy.lt.eps) .and. (valz.lt.eps)) then
                dir = 'DX'
                axe = .true.
            endif
            if ((valx.lt.eps) .and. (valz.lt.eps)) then
                dir = 'DY'
                axe = .true.
            endif
            if ((valx.lt.eps) .and. (valy.lt.eps)) then
                dir = 'DZ'
                axe = .true.
            endif
!
            if (axe) then
                do 250 icmp = 1, ncmp2
                    if (zk8(jcns2c-1+icmp) .eq. dir) then
                        icmpd=icmp
                        goto 260
                    endif
250              continue
260              continue
!
                zl(jcns2l-1+ (ino2-1)*ncmp2+icmpd)=.true.
                if (tsca .eq. 'R') then
                    zr(jcns2v-1+ (ino2-1)*ncmp2+icmpd)=v2
                else
                    zc(jcns2v-1+ (ino2-1)*ncmp2+icmpd)=v2c
                endif
            endif
        endif
!
100  continue
!
    call jedetr(trav)
    call jedetr(licmp)
!
    iret = 0
!
9999  continue
    call jedema()
end subroutine
