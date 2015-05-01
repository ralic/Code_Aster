subroutine cnsfus(nbchs, lichs, lcumul, lcoefr, lcoefc,&
                  lcoc, base, cns3z)
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnscre.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbchs
    character(len=*) :: lichs(nbchs), cns3z, base
    aster_logical :: lcumul(nbchs), lcoc
    real(kind=8) :: lcoefr(nbchs)
    complex(kind=8) :: lcoefc(nbchs)
! ---------------------------------------------------------------------
! BUT: FUSIONNER UNE LISTE DE CHAM_NO_S POUR EN FORMER 1 AUTRE
! ---------------------------------------------------------------------
!     ARGUMENTS:
! NBCHS   IN       I      : NOMBRE DE CHAM_NO_S A FUSIONNER
! LICHS   IN/JXIN  V(K19) : LISTE DES SD CHAM_NO_S A FUSIONNER
! LCUMUL  IN       V(L)   : V(I) =.TRUE. => ON ADDITIONNE LE CHAMP I
!                         : V(I) =.FALSE. => ON SURCHARGE LE CHAMP I
! LCOEFR  IN       V(R)   : LISTE DES COEF. MULT. DES VALEURS DES CHAMPS
! LCOEFC  IN       V(C)   : LISTE DES COEF. MULT. DES VALEURS DES CHAMPS
! LCOC    IN       L      : =TRUE SI COEF COMPLEXE
! CNS3Z   IN/JXOUT K19 : SD CHAM_NO_S RESULTAT
! BASE    IN       K1  : BASE DE CREATION POUR CNS3Z : G/V/L
!
! REMARQUES :
!
!  - LES CHAM_NO_S DE LICHS DOIVENT ETRE DE LA MEME GRANDEUR
!    ET S'APPUYER SUR LE MEME MAILLAGE
!
!  - L'ORDRE DES CHAM_NO_S DANS LICHS EST IMPORTANT :
!      LES CHAM_NO_S SE SURCHARGENT LES UNS LES AUTRES
!
!  - ON PEUT APPELER CETTE ROUTINE MEME SI CNS3Z APPARTIENT
!    A LA LISTE LICHS (CHAM_NO_S IN/OUT)
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jcn1k, jcn1d, jcn1v, jcn1l, jcn1c, nbno
    integer :: jcn3d, jcn3v, jcn3l
    integer :: jcmpgd, ichs, icmp, icmp3, ncmp3
    integer :: ncmpmx, ncmp1, icmp1
    integer :: ino, coefi, k1, k3
    character(len=8) :: ma, nomgd, nocmp
    character(len=3) :: tsca
    character(len=19) :: cns1, cns3
    real(kind=8) :: coefr
    complex(kind=8) :: coefc
    aster_logical :: cumul
    character(len=8), pointer :: cn3c(:) => null()
    character(len=8), pointer :: licmp(:) => null()
    integer, pointer :: nucmp(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
!     -- POUR NE PAS RISQUER D'ECRASER UN CHAM_NO_S "IN",
!        ON CREE CNS3 SOUS UN NOM TEMPORAIRE :
    cns3 = '&&CNSFUS.CNS3'
    ASSERT(nbchs.gt.0)
!
    cns1 = lichs(1)
!
    call jeveuo(cns1//'.CNSK', 'L', jcn1k)
    call jeveuo(cns1//'.CNSD', 'L', jcn1d)
!
    ma = zk8(jcn1k-1+1)
    nomgd = zk8(jcn1k-1+2)
    nbno = zi(jcn1d-1+1)
    ncmp1 = zi(jcn1d-1+2)
!
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmpgd)
!
!
!     1- CALCUL DE LA LISTE DES CMPS DE CNS3 :
!     ---------------------------------------
!
!     -- ON "COCHE" LES CMPS PRESENTES DANS LES CNS DE LICHS:
    AS_ALLOCATE(vk8=licmp, size=ncmpmx)
    AS_ALLOCATE(vi=nucmp, size=ncmpmx)
    do ichs = 1, nbchs
        cns1 = lichs(ichs)
        call jeveuo(cns1//'.CNSK', 'L', jcn1k)
        call jeveuo(cns1//'.CNSD', 'L', jcn1d)
        call jeveuo(cns1//'.CNSC', 'L', jcn1c)
!
!       TEST SUR IDENTITE DES 2 MAILLAGES
        ASSERT(ma.eq.zk8(jcn1k-1+1))
!       TEST SUR IDENTITE DES 2 GRANDEURS
        ASSERT(nomgd.eq.zk8(jcn1k-1+2))
!
        ncmp1 = zi(jcn1d-1+2)
        do icmp1 = 1, ncmp1
            nocmp = zk8(jcn1c-1+icmp1)
            icmp = indik8(zk8(jcmpgd),nocmp,1,ncmpmx)
            nucmp(icmp) = 1
        end do
        call jelibe(cns1//'.CNSK')
        call jelibe(cns1//'.CNSD')
        call jelibe(cns1//'.CNSC')
    end do
!
    icmp3 = 0
    do icmp = 1, ncmpmx
        if (nucmp(icmp) .eq. 1) then
            icmp3 = icmp3 + 1
            licmp(icmp3) = zk8(jcmpgd-1+icmp)
        endif
    end do
    ncmp3 = icmp3
!
!
!     2- ALLOCATION DE CNS3 :
!     ---------------------------------------
    call cnscre(ma, nomgd, ncmp3, licmp, base,&
                cns3)
    call jeveuo(cns3//'.CNSD', 'L', jcn3d)
    call jeveuo(cns3//'.CNSC', 'L', vk8=cn3c)
    call jeveuo(cns3//'.CNSV', 'E', jcn3v)
    call jeveuo(cns3//'.CNSL', 'E', jcn3l)
!
!
!     2- RECOPIE DE CNS1 DANS CNS3 :
!     ------------------------------------------
    do ichs = 1, nbchs
        cns1 = lichs(ichs)
!
        cumul = lcumul(ichs)
        if (lcoc) then
            coefc = lcoefc(ichs)
        else
            coefr = lcoefr(ichs)
            if (tsca .eq. 'I') coefi = nint(coefr)
        endif
!
        call jeveuo(cns1//'.CNSD', 'L', jcn1d)
        call jeveuo(cns1//'.CNSC', 'L', jcn1c)
        call jeveuo(cns1//'.CNSV', 'L', jcn1v)
        call jeveuo(cns1//'.CNSL', 'L', jcn1l)
        ncmp1 = zi(jcn1d-1+2)
!
        do icmp1 = 1, ncmp1
            nocmp = zk8(jcn1c-1+icmp1)
            icmp3 = indik8(cn3c,nocmp,1,ncmp3)
            ASSERT(icmp3.ne.0)
!
            do ino = 1, nbno
                k1 = (ino-1)*ncmp1 + icmp1
                k3 = (ino-1)*ncmp3 + icmp3
!
!
                if (zl(jcn1l-1+k1)) then
!
!             -- SI AFFECTATION :
                    if ((.not.cumul) .or. (.not.zl(jcn3l-1+k3))) then
                        zl(jcn3l-1+k3) = .true.
!
                        if (tsca .eq. 'R') then
                            zr(jcn3v-1+k3) = coefr*zr(jcn1v-1+k1)
                        else if (tsca.eq.'I') then
                            zi(jcn3v-1+k3) = coefi*zi(jcn1v-1+k1)
                        else if (tsca.eq.'L') then
                            zl(jcn3v-1+k3) = zl(jcn1v-1+k1)
                        else if (tsca.eq.'C') then
                            if (lcoc) then
                                zc(jcn3v-1+k3) = coefc*zc(jcn1v-1+k1)
                            else
                                zc(jcn3v-1+k3) = coefr*zc(jcn1v-1+k1)
                            endif
                        else if (tsca.eq.'K8') then
                            zk8(jcn3v-1+k3) = zk8(jcn1v-1+k1)
                        else
                            ASSERT(.false.)
                        endif
!
!             -- SI CUMUL DANS UNE VALEUR DEJA AFFECTEE :
                    else
                        if (tsca .eq. 'R') then
                            zr(jcn3v-1+k3) = zr(jcn3v-1+k3) + coefr* zr(jcn1v-1+k1)
                        else if (tsca.eq.'C') then
                            if (lcoc) then
                                zc(jcn3v-1+k3) = zc(jcn3v-1+k3) + coefc*zc(jcn1v-1+k1)
                            else
                                zc(jcn3v-1+k3) = zc(jcn3v-1+k3) + coefr*zc(jcn1v-1+k1)
                            endif
                        else if (tsca.eq.'I') then
                            zi(jcn3v-1+k3) = zi(jcn3v-1+k3) + coefi* zi(jcn1v-1+k1)
                            else if ((tsca.eq.'L') .or. (tsca.eq.'K8'))&
                        then
!                 CUMUL INTERDIT SUR CE TYPE NON-NUMERIQUE
                            ASSERT(.false.)
                        else
                            ASSERT(.false.)
                        endif
                    endif
!
                endif
!
            end do
        end do
!
        call jelibe(cns1//'.CNSD')
        call jelibe(cns1//'.CNSC')
        call jelibe(cns1//'.CNSV')
        call jelibe(cns1//'.CNSL')
!
    end do
!
!
!     3- RECOPIE DE LA SD TEMPORAIRE DANS LE RESULTAT :
!     -------------------------------------------------
    call copisd('CHAM_NO_S', base, cns3, cns3z)
!
!
!     4- MENAGE :
!     -----------
    call detrsd('CHAM_NO_S', cns3)
    AS_DEALLOCATE(vk8=licmp)
    AS_DEALLOCATE(vi=nucmp)
!
    call jedema()
end subroutine
