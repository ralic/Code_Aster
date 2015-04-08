subroutine vrcins(modelz, chmatz, carelz, inst, chvarc,&
                  codret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcin1.h"
#include "asterfort/vrcin2.h"
#include "asterfort/detrsd.h"
!
    character(len=2) :: codret
    character(len=19) :: chvarc
    character(len=*) :: chmatz, carelz, modelz
    real(kind=8) :: inst
! ======================================================================
!   BUT : FABRIQUER LE CHAMP DE VARIABLES DE COMMANDE CORRESPONDANT A
!         UN INSTANT DONNE.
!   ARGUMENTS :
!   MODELZ (K8)  IN/JXIN : SD MODELE
!   CHMATZ (K8)  IN/JXIN : SD CHAM_MATER
!   CARELZ (K8)  IN/JXIN : SD CARA_ELEM (SOUS-POINTS)
!   INST   (R)   IN      : VALEUR DE L'INSTANT
!   CHVARC (K19) IN/JXOUT: SD CHAM_ELEM/ELGA CONTENANT LES VARC
!   CODRET (K2)  OUT : POUR CHAQUE RESULTAT, 'OK' SI ON A TROUVE,
!                                            'NO' SINON
!
!
! ----------------------------------------------------------------------
!
!
    integer :: iret, ichs, nbchs, jcesd, jcesl
    integer :: nbcmp, kcmp, kcvrc
    integer :: nbma, ima, nbpt, nbsp, ipt, isp, iad, iad1
    integer :: jce1d, jce1l, nncp, n1, k
    real(kind=8) :: valeur, rundef
    character(len=19) :: chvars, ligrmo, chs
    character(len=24) :: valk(5)
    character(len=16) :: nomte
    aster_logical :: avrc, dbg
    integer :: ibid, nbcvrc, nute, jmaille, vali(2)
    character(len=8) :: modele, chmat, carele, varc1, varc2, nocmp1, nocmp2, noma, nomail
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=24), pointer :: liste_ch(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    character(len=16), pointer :: liste_sd(:) => null()
    character(len=8), pointer :: cvrccmp(:) => null()
    integer, pointer :: cesvi(:) => null()
    real(kind=8), pointer :: ce1v(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    aster_logical :: exival
! ----------------------------------------------------------------------
!
    call jemarq()
!
    chmat=chmatz
    carele=carelz
    modele=modelz
    call detrsd('CHAM_ELEM', chvarc)
!
!
    call jeexin(chmat//'.CVRCVARC', iret)
!     AVRC : .TRUE. SI AFFE_MATERIAU/AFFE_VARC EST UTILISE
    avrc=(iret.gt.0)
    if (.not.avrc) goto 9999
!
!
!   1. interpolation en temps :
!      fabrication d'une liste de cham_elem_s / elga
!      contenant les vrc a l'instant inst
!      calcul de  chmat.liste_ch(:) et chmat.liste_sd(:)
!   -----------------------------------------------------
    call vrcin1(modele, chmat, carele, inst, codret)

!   1.1 si il n'y a pas vraiment de variables de commande
!       (par exemple il existe temp/vale_ref mais pas de temp
    call jeexin(chmat//'.LISTE_SD', iret)
    if (iret .eq. 0) goto 9999


!   2. allocation du champ_elem_s resultat (chvars)
!      calcul de chmat.cesvi
!      (cette etape est economisee d'un instant a l'autre)
!   -------------------------------------------------------------
    chvars=chmat//'.CHVARS'
    call jeexin(chmat//'.CESVI', iret)
    if (iret .eq. 0) call vrcin2(modele, chmat, carele, chvars)
!
!
!   3. concatenation des champs de .liste_ch  dans chvars :
!   -----------------------------------------------------
    call jeveuo(chmat//'.LISTE_CH', 'L', vk24=liste_ch)
    call jelira(chmat//'.LISTE_CH', 'LONMAX', nbchs)
    call jeveuo(chmat//'.LISTE_SD', 'L', vk16=liste_sd)
    call jeveuo(chmat//'.CVRCVARC', 'L', vk8=cvrcvarc)
    call jeveuo(chmat//'.CVRCCMP', 'L', vk8=cvrccmp)
    call jelira(chmat//'.CVRCCMP', 'LONMAX', nbcvrc)
!
    call jeveuo(chvars//'.CESD', 'L', jce1d)
    call jeveuo(chvars//'.CESL', 'E', jce1l)
    call jeveuo(chmat//'.CESVI', 'L', vi=cesvi)

!   -- il faut remettre cesv a nan:
    rundef=r8nnem()
    call jeveuo(chvars//'.CESV', 'E', vr=ce1v)
    call jelira(chvars//'.CESV', 'LONMAX', n1)
    do k = 1, n1
        ce1v(k)=rundef
    end do


    do 1 ichs = 1, nbchs
        chs=liste_ch(ichs)(1:19)
        varc1=liste_sd(7*(ichs-1)+4)(1:8)
        call jeveuo(chs//'.CESD', 'L', jcesd)
        call jeveuo(chs//'.CESL', 'L', jcesl)
        call jeveuo(chs//'.CESV', 'L', vr=cesv)
        call jeveuo(chs//'.CESC', 'L', vk8=cesc)
        call jelira(chs//'.CESC', 'LONMAX', nbcmp)
!
        do 2 kcmp = 1, nbcmp
            nocmp1=cesc(kcmp)
!
!         -- calcul de kcvrc :
            do 3 kcvrc = 1, nbcvrc
                varc2=cvrcvarc(kcvrc)
                nocmp2=cvrccmp(kcvrc)
                if ((varc1.eq.varc2) .and. (nocmp1.eq.nocmp2)) goto 4
  3         continue
            goto 2
!
  4         continue
            ASSERT(kcvrc.ge.1 .and. kcvrc.le.nbcvrc)

!           -- boucle sur les mailles :
            nbma = zi(jcesd-1+1)
            ASSERT(nbma.eq.zi(jce1d-1+1))

            do 70 ima = 1, nbma
                nbpt = zi(jcesd-1+5+4* (ima-1)+1)
                nbsp = zi(jcesd-1+5+4* (ima-1)+2)
                if (nbsp*nbsp .eq. 0) goto 70

                call cesexi('C', jce1d, jce1l, ima, 1, 1, kcvrc, iad1)
!               -- la maille n'est pas concernee par les variables de commande :
                if (iad1 .eq. 0)  goto 70


!               -- la maille porte un element fini qui saurait utiliser
!                  les variables de commande mais elle n'est pas affectee.
!                  on espere que les routines te00ij arreteront en <f> si necessaire.
                if (iad1 .lt. 0) goto 70

!               -- On regarde si le champ possede des valeurs sur la maille :
                exival=.false.
                do ipt = 1, nbpt
                    do isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    isp, kcmp, iad)
                        if (iad .gt. 0) exival=.true.
                    enddo
                enddo
                if (.not.exival) goto 70

!               -- controle du nombre de points :
                ASSERT(nbpt.eq.zi(jce1d-1+5+4* (ima-1)+1))

!               -- Controle du nombre de sous-points :
                if (nbsp .ne. zi(jce1d-1+5+4* (ima-1)+2)) then
!                   -- issue23456 : il peut arriver que nbsp=1 mais sans aucune valeur :
                    if (nbsp.eq.1 .and. .not.exival) goto 70

                    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
                    call jenuno(jexnum(noma//'.NOMMAI',ima), nomail)
                    call jeveuo(modele//'.MAILLE', 'L', jmaille)
                    nute=zi(jmaille-1+ima)
                    call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
                    valk(1) = nocmp1
                    valk(2) = carele
                    valk(3) = chmat
                    valk(4) = nomail
                    valk(5) = nomte
                    vali(1) = zi(jce1d-1+5+4* (ima-1)+2)
                    vali(2) = nbsp
                    call utmess('F', 'CALCULEL6_57', nk=5, valk=valk, ni=2, vali=vali)
                endif


                do 60 ipt = 1, nbpt
                    do 50 isp = 1, nbsp
                        call cesexi('C', jcesd, jcesl, ima, ipt,&
                                    isp, kcmp, iad)
                        if (iad .gt. 0) then
                            call cesexi('C', jce1d, jce1l, ima, ipt,&
                                        isp, kcvrc, iad1)
                            ASSERT(iad1.gt.0)
                            if (cesvi(iad1) .eq. ichs) then
                                valeur=cesv(iad)
                                zl(jce1l-1+iad1)=.true.
                                ce1v(iad1)=valeur
                            endif
                        endif
 50                 continue
 60             continue
 70         continue

  2     continue
  1 end do


!   4. recopie du champ simple dans le champ chvarc
!   -----------------------------------------------------
    ligrmo=modele//'.MODELE'
    call cescel(chvars, ligrmo, 'INIT_VARC', 'PVARCPR', 'NAN',&
                nncp, 'V', chvarc, 'F', ibid)

    dbg=.false.
    if (dbg) call imprsd('CHAMP', chvarc, 6, 'VRCINS/CHVARC')

9999 continue
    call jedema()
end subroutine
