subroutine cnsprj(cns1z, correz, basez, cns2z, iret)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: cns1z, correz, basez, cns2z
    integer :: iret
! ------------------------------------------------------------------
! BUT : PROJETER UN CHAM_NO_S  SUR UN AUTRE MAILLAGE
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNS1Z  IN/JXIN  K19 : CHAM_NO_S A PROJETER
! CORREZ IN/JXIN  K16 : NOM DE LA SD CORRESP_2_MAILLA
! BASEZ  IN       K1  : BASE DE CREATION POUR CNS2Z : G/V
! CNS2Z  IN/JXOUT K19 : CHAM_NO_S RESULTAT DE LA PROJECTION
! IRET   OUT      I   : CODE RETOUR :
!                       0 -> OK
!                       1 -> ECHEC DE LA PROJECTION
! ------------------------------------------------------------------
!    ON NE TRAITE QUE LES CHAMPS REELS (R8) OU COMPLEXES (C16)
!
!
! REMARQUE :
!   LA PROJECTION EST APPROCHEE DANS LE CAS OU TOUS LES NOEUDS
!   DE LA MAILLE M1 NE PORTENT PAS LES MEMES DDLS.
!   LE TRAITEMENT EST EXPLIQUE DANS LA REPONSE A LA FICHE 9259
!
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: base
    character(len=3) :: tsca
    character(len=8) :: ma1, ma2, nomgd, nomcmp, nomno2
    character(len=16) :: corres
    character(len=19) :: cns1, cns2
    integer :: jcns1c, jcns1l, jcns1v, jcns1k, jcns1d
    integer :: jcns2c, jcns2l, jcns2v, jcns2k, jcns2d
    integer :: nbno1, ncmp, ibid, jxxk1, iaconb, iaconu, iacocf, gd, nbno2
    integer :: idecal, ino2, icmp, ico1, ico2, ino1, nuno1, kalarm
    real(kind=8) :: v1, v2, coef1, coetot, vrmoy
    complex(kind=8) :: v1c, v2c, vcmoy
    logical :: lexact
!     ------------------------------------------------------------------
!
    call jemarq()
    cns1 = cns1z
    cns2 = cns2z
    base = basez
    corres = correz
    iret = 0
    kalarm=0
!
!
!------------------------------------------------------------------
!     1- RECUPERATION DES OBJETS ET INFORMATIONS DE CNS1 :
!     ----------------------------------------------------
!
    call jeveuo(cns1//'.CNSK', 'L', jcns1k)
    call jeveuo(cns1//'.CNSD', 'L', jcns1d)
    call jeveuo(cns1//'.CNSC', 'L', jcns1c)
    call jeveuo(cns1//'.CNSV', 'L', jcns1v)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
!
    ma1 = zk8(jcns1k-1+1)
    nomgd = zk8(jcns1k-1+2)
    nbno1 = zi(jcns1d-1+1)
    ncmp = zi(jcns1d-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
!
!------------------------------------------------------------------
!     2- RECUPERATION DES OBJETS ET INFORMATIONS DE CORRES :
!     ----------------------------------------------------
    call jeveuo(corres//'.PJXX_K1', 'L', jxxk1)
    call jeveuo(corres//'.PJEF_NB', 'L', iaconb)
    call jeveuo(corres//'.PJEF_NU', 'L', iaconu)
    call jeveuo(corres//'.PJEF_CF', 'L', iacocf)
!
    ma2 = zk24(jxxk1-1+2)
!
!
!------------------------------------------------------------------
!     3- QUELQUES VERIFS :
!     ------------------------
    if (tsca .ne. 'R' .and. tsca .ne. 'C') then
!        -- ON NE TRAITE QUE LES CHAMPS R/C :
        iret = 1
        goto 60
!
    endif
!     TEST SUR IDENTITE DES 2 MAILLAGES
    ASSERT(zk24(jxxk1-1+1).eq.ma1)
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) then
        call utmess('F', 'CALCULEL_67', sk=nomgd)
    endif
!
!
!------------------------------------------------------------------
!     4- ALLOCATION DE CNS2 :
!     ------------------------
    call detrsd('CHAM_NO_S', cns2)
    call cnscre(ma2, nomgd, ncmp, zk8(jcns1c), base,&
                cns2)
    call jeveuo(cns2//'.CNSK', 'L', jcns2k)
    call jeveuo(cns2//'.CNSD', 'L', jcns2d)
    call jeveuo(cns2//'.CNSC', 'L', jcns2c)
    call jeveuo(cns2//'.CNSV', 'E', jcns2v)
    call jeveuo(cns2//'.CNSL', 'E', jcns2l)
!
    nbno2 = zi(jcns2d-1+1)
!
!------------------------------------------------------------------
!     5- CALCUL DES VALEURS DE CNS2 :
!     -------------------------------
    idecal = 0
    do 50,ino2 = 1,nbno2
    nbno1 = zi(iaconb-1+ino2)
    if (nbno1 .eq. 0) goto 50
    do 40,icmp = 1,ncmp
!
!          -- ON COMPTE (ICO1) LES NOEUDS PORTANT LE DDL :
!             ON COMPTE AUSSI (ICO2) CEUX DONT LE COEF EST > 0
!             ON CALCULE LA VALEUR MOYENNE SUR LA MAILLE (VXMOY)
!             ON CALCULE LA SOMME DES COEF > 0 (COETOT)
    ico1 = 0
    ico2 = 0
    vrmoy = 0.d0
    vcmoy = dcmplx(0.d0,0.d0)
    coetot = 0.d0
    do 10,ino1 = 1,nbno1
    nuno1 = zi(iaconu+idecal-1+ino1)
    coef1 = zr(iacocf+idecal-1+ino1)
    if (zl(jcns1l-1+ (nuno1-1)*ncmp+icmp)) then
        ico1 = ico1 + 1
        if (coef1 .gt. 0.d0) then
            ico2 = ico2 + 1
            coetot = coetot + coef1
        endif
        if (tsca .eq. 'R') then
            vrmoy = vrmoy + zr(jcns1v-1+ (nuno1-1)*ncmp+ icmp)
!
        else
            vcmoy = vcmoy + zc(jcns1v-1+ (nuno1-1)*ncmp+ icmp)
        endif
    endif
10  continue
    if (ico1 .eq. 0) goto 40
    zl(jcns2l-1+ (ino2-1)*ncmp+icmp) = .true.
!
!
!         -- SI COETOT EST FAIBLE, LA PROJECTION N'EST PAS PRECISE :
!            L'EMISSION DE L'ALARME EST COUTEUSE, ON LA LIMITE :
    if (coetot .lt. 1.d-3 .and. kalarm .le. 6) then
        kalarm=kalarm+1
        call jenuno(jexnum(ma2//'.NOMNOE', ino2), nomno2)
        nomcmp=zk8(jcns1c-1+icmp)
        valk(1)=nomgd
        valk(2)=nomno2
        valk(3)=nomcmp
        call utmess('A', 'CALCULEL4_9', nk=3, valk=valk)
    endif
!
!
!          -- 3 CAS DE FIGURE POUR L'INTERPOLATION :
!          ----------------------------------------
    if (ico1 .eq. nbno1) then
!            1 : NORMAL ON PREND TOUS LES NOEUDS N1
        lexact = .true.
        coetot = 1.d0
!
    else if (ico2.gt.0) then
!            2 : ON PREND LES NOEUDS N1 DE COEF > 0
        lexact = .false.
!
    else
!            3 : ON FAIT UNE MOYENNE ARITHMETIQUE
        if (tsca .eq. 'R') then
            zr(jcns2v-1+ (ino2-1)*ncmp+icmp) = vrmoy/ico1
!
        else
            zc(jcns2v-1+ (ino2-1)*ncmp+icmp) = vcmoy/ico1
        endif
        goto 40
!
    endif
!
!
    if (tsca .eq. 'R') then
        v2 = 0.d0
        do 20,ino1 = 1,nbno1
        nuno1 = zi(iaconu+idecal-1+ino1)
        coef1 = zr(iacocf+idecal-1+ino1)
        if (zl(jcns1l-1+ (nuno1-1)*ncmp+icmp)) then
            if (lexact .or. coef1 .gt. 0) then
                v1 = zr(jcns1v-1+ (nuno1-1)*ncmp+icmp)
                v2 = v2 + coef1*v1
            endif
        endif
20      continue
        zr(jcns2v-1+ (ino2-1)*ncmp+icmp) = v2/coetot
!
    else if (tsca.eq.'C') then
        v2c = dcmplx(0.d0,0.d0)
        do 30,ino1 = 1,nbno1
        nuno1 = zi(iaconu+idecal-1+ino1)
        coef1 = zr(iacocf+idecal-1+ino1)
        if (zl(jcns1l-1+ (nuno1-1)*ncmp+icmp)) then
            if (lexact .or. coef1 .gt. 0) then
                v1c = zc(jcns1v-1+ (nuno1-1)*ncmp+icmp)
                v2c = v2c + coef1*v1c
            endif
        endif
30      continue
        zc(jcns2v-1+ (ino2-1)*ncmp+icmp) = v2c/coetot
    endif
40  continue
    idecal = idecal + nbno1
    50 end do
!
60  continue
    call jedema()
end subroutine
