subroutine rc36ma(nommat, noma)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/r8vide.h"
#include "asterfort/carces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rccome.h"
#include "asterfort/rcvale.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nommat, noma
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     TRAITEMENT DU CHAM_MATER
!     RECUPERATION POUR CHAQUE MAILLE
!          DE  E, NU, ALPHA    SOUS ELAS
!          DE  E_REFE          SOUS FATIGUE
!          DE  M_KE, N_KE, SM  SOUS RCCM
!
! IN  : NOMMAT : CHAM_MATER UTILISATEUR
! IN  : NOMA   : MAILLAGE
!
!     CREATION DES OBJETS :
!
!     &&RC3600.MATERIAU' : VECTEUR DE DIMENSION 2*NOMBRE DE SITUATIONS
!         CONTENANT LES NOMS DES CHAM_ELEM  CHMATA ET CHMATB :
!
!         CHMATA = '&&RC36MA_A.'//NUMSITU : CHAMP SIMPLE ELNO CONTENANT
!              POUR CHAQUE MAILLE LES 8 PARAMETRES
!                    E_AMBI, NU, ALPHA : A LA TEMPERATURE DE REFERENCE
!                    E, SM, M_KE, N_KE : A LA TEMPERATURE DE CALCUL
!
!         CHMATB = '&&RC36MA_B.'//NUMSITU : CHAMP SIMPLE ELNO CONTENANT
!              POUR CHAQUE MAILLE LES 8 PARAMETRES
!                    E_AMBI, NU, ALPHA : A LA TEMPERATURE DE REFERENCE
!                    E, SM, M_KE, N_KE : A LA TEMPERATURE DE CALCUL
!
!      &&RC3600.NOM_MATERIAU : VECTEUR DIMENSIONNE A NBMAIL CONTENANT
!      POUR CHAQUE MAILLE LE NOM DU MATERIAU ASSOCIE (POUR RECUPER LA
!      COURBE DE FATIGUE)
!-----------------------------------------------------------------------
!     ------------------------------------------------------------------
!
    integer :: nbcmp
    parameter (nbcmp=9)
    integer :: nbpa, nbpb, nbpt, ipt, nbseis, ndim,  jcesdm, jceslm, isp
    integer :: icmp, iad, im, nbmail, jcesla, jcesva, jcesda, jceslb
    integer :: jcesvb, jcesdb, ier, iocc, nbsitu, jchmat, na, nb, jmater
    real(kind=8) :: para(nbcmp), tempa, tempra, tempb, temprb, tke
    integer :: icodre(nbcmp)
    character(len=8) :: k8b, nomgd, mater, nopa, nopb, typeke, nocmp(nbcmp)
    character(len=8) :: licmp(2), ktref
    character(len=16) :: motcl1, motcl2
    character(len=19) :: chnmat, chsmat, chsma2
    character(len=24) :: chmata, chmatb
    character(len=8), pointer :: cesvm(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    motcl1 = 'SITUATION'
    motcl2 = 'SEISME'
    call getfac(motcl1, nbsitu)
    call getfac(motcl2, nbseis)
    ndim = nbsitu + nbseis
!
!    RECUP TYPE KE
    call getvtx(' ', 'TYPE_KE', scal=typeke, nbret=nb)
    if (typeke .eq. 'KE_MECA') then
        tke=-1.d0
    else
        tke=1.d0
    endif
    para(9)=tke
!
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmail)
!
    call wkvect('&&RC3600.MATERIAU', 'V V K24', 2*ndim, jchmat)
    call wkvect('&&RC3600.NOM_MATERIAU', 'V V K8', nbmail, jmater)
!
    chnmat = nommat//'.CHAMP_MAT '
    chsmat = '&&RC36MA.CHSMAT'
    chsma2 = '&&RC36MA.CHSMA2'
    k8b = ' '
    call carces(chnmat, 'ELEM', k8b, 'V', chsmat,&
                'A', ier)
    if (ier .ne. 0) then
        call utmess('F', 'POSTRCCM_13')
    endif
!     -- ON NE GARDE DANS LE CHAMP DE MATERIAU QUE LES CMPS
!      X1 : NOM DU MATERIAU
!      X30: TEMP_REF
    licmp(1)='MAT1'
    licmp(2)='VREF'
    call cesred(chsmat, 0, [0], 2, licmp,&
                'V', chsma2)
    call detrsd('CHAM_ELEM_S', chsmat)
!
    call jeveuo(chsma2//'.CESV', 'L', vk8=cesvm)
    call jeveuo(chsma2//'.CESD', 'L', jcesdm)
    call jeveuo(chsma2//'.CESL', 'L', jceslm)
!
    if (ier .ne. 0) then
        call utmess('F', 'POSTRCCM_14')
    endif
!
!
! --- E_AMBI, NU, ALPHA : A LA TEMPERATURE DE REFERENCE
! --- E, SM, M_KE, N_KE : A LA TEMPERATURE DE CALCUL
!
    nomgd = 'RCCM_R'
    nocmp(1) = 'E'
    nocmp(2) = 'E_AMBI'
    nocmp(3) = 'NU'
    nocmp(4) = 'ALPHA'
    nocmp(5) = 'E_REFE'
    nocmp(6) = 'SM'
    nocmp(7) = 'M_KE'
    nocmp(8) = 'N_KE'
    nocmp(9) = 'TYPEKE'
!
    do iocc = 1, nbsitu, 1
!
        call codent(iocc, 'D0', k8b)
!
! ------ ETAT STABILISE "A"
!        ------------------
!
        chmata = '&&RC36MA_A.'//k8b
        nocmp(2) = 'E_AMBI'
        call cescre('V', chmata, 'ELNO', noma, nomgd,&
                    nbcmp, nocmp, [-1], [-1], [-nbcmp])
        nocmp(2) = 'E'
!
        call jeveuo(chmata(1:19)//'.CESD', 'L', jcesda)
        call jeveuo(chmata(1:19)//'.CESL', 'E', jcesla)
        call jeveuo(chmata(1:19)//'.CESV', 'E', jcesva)
!
        nbpa = 1
        nopa = 'TEMP'
        call getvr8(motcl1, 'TEMP_REF_A', iocc=iocc, scal=tempa, nbret=na)
!
! ------ ETAT STABILISE "B"
!        ------------------
!
        chmatb = '&&RC36MA_B.'//k8b
        nocmp(2) = 'E_AMBI'
        call cescre('V', chmatb, 'ELNO', noma, nomgd,&
                    nbcmp, nocmp, [-1], [-1], [-nbcmp])
        nocmp(2) = 'E'
!
        call jeveuo(chmatb(1:19)//'.CESD', 'L', jcesdb)
        call jeveuo(chmatb(1:19)//'.CESL', 'E', jceslb)
        call jeveuo(chmatb(1:19)//'.CESV', 'E', jcesvb)
!
        nbpb = 1
        nopb = 'TEMP'
        call getvr8(motcl1, 'TEMP_REF_B', iocc=iocc, scal=tempb, nbret=nb)
!
        do im = 1, nbmail
!
            icmp = 1
!
! --------- LE MATERIAU
            call cesexi('C', jcesdm, jceslm, im, 1,&
                        1, 1, iad)
            if (iad .gt. 0) then
                mater = cesvm(iad)
            else
                call codent(im, 'D', k8b)
                call utmess('F', 'POSTRCCM_10', sk=k8b)
            endif
!
! --------- LA TEPERATURE DE REFERENCE :
            call cesexi('C', jcesdm, jceslm, im, 1,&
                        1, 2, iad)
            if (iad .gt. 0) then
                ktref = cesvm(iad)
                if (ktref .eq. 'NAN') then
                    tempra=r8vide()
                else
                    read (ktref,'(F8.2)') tempra
                endif
            else
                tempra=r8vide()
            endif
!
            if (na .eq. 0) tempa = tempra
            temprb = tempra
            if (nb .eq. 0) tempb = temprb
!
            zk8(jmater+im-1) = mater
            call rccome(mater, 'ELAS', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='ELAS')
            endif
!
            call rccome(mater, 'FATIGUE', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='FATIGUE')
            endif
!
            call rccome(mater, 'RCCM', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='RCCM')
            endif
!
!   INTERPOLATION POUR TEMP_A
            call rcvale(mater, 'ELAS', nbpa, nopa, [tempa],&
                        1, nocmp(1), para( 1), icodre, 2)
!
            call rcvale(mater, 'ELAS', nbpa, nopa, [tempra],&
                        3, nocmp(2), para( 2), icodre, 2)
!
            call rcvale(mater, 'FATIGUE', nbpa, nopa, [tempa],&
                        1, nocmp(5), para(5), icodre, 2)
!
            call rcvale(mater, 'RCCM', nbpa, nopa, [tempa],&
                        3, nocmp(6), para( 6), icodre, 2)
!
! --------- LES MAILLES AFFECTEES
!
            nbpt = zi(jcesda-1+5+4* (im-1)+1)
            isp = 1
            do ipt = 1, nbpt
                do icmp = 1, nbcmp
                    call cesexi('S', jcesda, jcesla, im, ipt,&
                                isp, icmp, iad)
                    if (iad .lt. 0) then
                        iad=-iad
                        zl(jcesla-1+iad) = .true.
                    endif
                    zr(jcesva-1+iad) = para(icmp)
                end do
            end do
!
!   INTERPOLATION POUR TEMP_B
            call rcvale(mater, 'ELAS', nbpb, nopb, [tempb],&
                        1, nocmp(1), para( 1), icodre, 2)
!
            call rcvale(mater, 'ELAS', nbpb, nopb, [temprb],&
                        3, nocmp(2), para( 2), icodre, 2)
!
            call rcvale(mater, 'FATIGUE', nbpb, nopb, [tempb],&
                        1, nocmp(5), para(5), icodre, 2)
!
            call rcvale(mater, 'RCCM', nbpb, nopb, [tempb],&
                        3, nocmp(6), para( 6), icodre, 2)
!
! --------- LES MAILLES AFFECTEES
!
            nbpt = zi(jcesdb-1+5+4* (im-1)+1)
            isp = 1
            do ipt = 1, nbpt
                do icmp = 1, nbcmp
                    call cesexi('S', jcesdb, jceslb, im, ipt,&
                                isp, icmp, iad)
                    if (iad .lt. 0) then
                        iad=-iad
                        zl(jceslb-1+iad) = .true.
                    endif
                    zr(jcesvb-1+iad) = para(icmp)
                end do
            end do
!
        end do
!
        zk24(jchmat+2*iocc-1) = chmata
!
        zk24(jchmat+2*iocc-2) = chmatb
!
    end do
!
    do iocc = 1, nbseis, 1
!
        call codent(nbsitu+iocc, 'D0', k8b)
!
! ------ ETAT STABILISE "A"
!        ------------------
!
        chmata = '&&RC36MA_A.'//k8b
        nocmp(2) = 'E_AMBI'
        call cescre('V', chmata, 'ELNO', noma, nomgd,&
                    nbcmp, nocmp, [-1], [-1], [-nbcmp])
        nocmp(2) = 'E'
!
        call jeveuo(chmata(1:19)//'.CESD', 'L', jcesda)
        call jeveuo(chmata(1:19)//'.CESL', 'E', jcesla)
        call jeveuo(chmata(1:19)//'.CESV', 'E', jcesva)
!
        nbpa = 1
        nopa = 'TEMP'
        call getvr8(motcl2, 'TEMP_REF', iocc=iocc, scal=tempa, nbret=na)
!
! ------ ETAT STABILISE "B"
!        ------------------
!
        chmatb = '&&RC36MA_B.'//k8b
        nocmp(2) = 'E_AMBI'
        call cescre('V', chmatb, 'ELNO', noma, nomgd,&
                    nbcmp, nocmp, [-1], [-1], [-nbcmp])
        nocmp(2) = 'E'
!
        call jeveuo(chmatb(1:19)//'.CESD', 'L', jcesdb)
        call jeveuo(chmatb(1:19)//'.CESL', 'E', jceslb)
        call jeveuo(chmatb(1:19)//'.CESV', 'E', jcesvb)
!
        nbpb = 1
        nopb = 'TEMP'
        call getvr8(motcl2, 'TEMP_REF', iocc=iocc, scal=tempb, nbret=nb)
!
        do im = 1, nbmail
!
            icmp = 1
!
! --------- LE MATERIAU
            call cesexi('C', jcesdm, jceslm, im, 1,&
                        1, 1, iad)
            if (iad .gt. 0) then
                mater = cesvm(iad)
            else
                call codent(im, 'D', k8b)
                call utmess('F', 'POSTRCCM_10', sk=k8b)
            endif
!
! --------- LA TEPERATURE DE REFERENCE :
            call cesexi('C', jcesdm, jceslm, im, 1,&
                        1, 2, iad)
            if (iad .gt. 0) then
                ktref = cesvm(iad)
                if (ktref .eq. 'NAN') then
                    tempra=r8vide()
                else
                    read (ktref,'(F8.2)') tempra
                endif
            else
                tempra=r8vide()
            endif
!
            if (na .eq. 0) tempa = tempra
            temprb = tempra
            if (nb .eq. 0) tempb = temprb
!
            zk8(jmater+im-1) = mater
            call rccome(mater, 'ELAS', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='ELAS')
            endif
!
            call rccome(mater, 'FATIGUE', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='FATIGUE')
            endif
!
            call rccome(mater, 'RCCM', icodre(1))
            if (icodre(1) .eq. 1) then
                call utmess('F', 'POSTRCCM_7', sk='RCCM')
            endif
!
!   INTERPOLATION POUR TEMP_A
            call rcvale(mater, 'ELAS', nbpa, nopa, [tempa],&
                        1, nocmp(1), para( 1), icodre, 2)
!
            call rcvale(mater, 'ELAS', nbpa, nopa, [tempra],&
                        3, nocmp(2), para( 2), icodre, 2)
!
            call rcvale(mater, 'FATIGUE', nbpa, nopa, [tempa],&
                        1, nocmp(5), para(5), icodre, 2)
!
            call rcvale(mater, 'RCCM', nbpa, nopa, [tempa],&
                        3, nocmp(6), para( 6), icodre, 2)
!
! --------- LES MAILLES AFFECTEES
!
            nbpt = zi(jcesda-1+5+4* (im-1)+1)
            isp = 1
            do ipt = 1, nbpt
                do icmp = 1, nbcmp
                    call cesexi('S', jcesda, jcesla, im, ipt,&
                                isp, icmp, iad)
                    if (iad .lt. 0) then
                        iad=-iad
                        zl(jcesla-1+iad) = .true.
                    endif
                    zr(jcesva-1+iad) = para(icmp)
                end do
            end do
!
!   INTERPOLATION POUR TEMP_B
            call rcvale(mater, 'ELAS', nbpb, nopb, [tempb],&
                        1, nocmp(1), para( 1), icodre, 2)
!
            call rcvale(mater, 'ELAS', nbpb, nopb, [temprb],&
                        3, nocmp(2), para( 2), icodre, 2)
!
            call rcvale(mater, 'FATIGUE', nbpb, nopb, [tempb],&
                        1, nocmp(5), para(5), icodre, 2)
!
            call rcvale(mater, 'RCCM', nbpb, nopb, [tempb],&
                        3, nocmp(6), para( 6), icodre, 2)
!
! --------- LES MAILLES AFFECTEES
!
            nbpt = zi(jcesdb-1+5+4* (im-1)+1)
            isp = 1
            do ipt = 1, nbpt
                do icmp = 1, nbcmp
                    call cesexi('S', jcesdb, jceslb, im, ipt,&
                                isp, icmp, iad)
                    if (iad .lt. 0) then
                        iad=-iad
                        zl(jceslb-1+iad) = .true.
                    endif
                    zr(jcesvb-1+iad) = para(icmp)
                end do
            end do
!
        end do
!
        zk24(jchmat+2*(nbsitu+iocc)-1) = chmata
!
        zk24(jchmat+2*(nbsitu+iocc)-2) = chmatb
!
    end do
    call detrsd('CHAM_ELEM_S', chsma2)
!
    call jedema()
end subroutine
