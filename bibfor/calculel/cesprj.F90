subroutine cesprj(ces1z, correz, basez, ces2z, iret)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesces.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesver.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: ces1z, correz, basez, ces2z
    integer :: iret
! ------------------------------------------------------------------
! BUT : PROJETER UN CHAM_ELEM_S  SUR UN AUTRE MAILLAGE
!       SI CES1 EST ELEM : CES2 SERA ELEM
!       SI CES1 EST ELNO : CES2 SERA ELNO
!       LES CMPS PORTEES PAR CES2 SERONT CELLES PORTEES PAR TOUS LES
!       NOEUDS DES MAILLES CONTENANT LES NOEUDS DES MAILLES DE CES2
! ------------------------------------------------------------------
!     ARGUMENTS:
! CES1Z  IN/JXIN  K19 : CHAM_ELEM_S A PROJETER
! CORREZ IN/JXIN  K16 : NOM DE LA SD CORRESP_2_MAILLA
! BASEZ  IN       K1  : BASE DE CREATION POUR CES2Z : G/V/L
! CES2Z  IN/JXOUT K19 : CHAM_ELEM_S RESULTAT DE LA PROJECTION
!                       S'IL EXISTE DEJA, ON LE DETRUIT
! IRET   OUT      I   : CODE RETOUR :
!                       0 -> OK
!                       1 -> ECHEC DE LA PROJECTION
! ------------------------------------------------------------------
!  RESTRICTIONS :
!    ON NE TRAITE QUE LES CHAMPS 'ELEM' ET 'ELNO'
!    ON NE TRAITE QUE LES CHAMPS REELS (R8) OU COMPLEXES (C16)
!
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: base
    character(len=3) :: tsca
    character(len=4) :: typces
    character(len=8) :: ma1, ma2, nomgd
    character(len=16) :: corres
    character(len=19) :: ces1, ces2
    integer ::  jce1l, jce1v, jce1k, jce1d
    integer :: jce2c, jce2l, jce2v, jce2d, ifm, niv
    integer :: nbno1,   iaconu,  gd, nbno2
    integer :: ncmpmx, iad1, iad2, ima1, ima2,  nbmam2
    integer :: idecal, ino2, icmp, ico, ino1, nuno2,  ilcnx2
    real(kind=8) :: v1, v2, coef1
    complex(kind=8) :: v1c, v2c
    integer, pointer :: pjef_nb(:) => null()
    integer, pointer :: connex(:) => null()
    character(len=24), pointer :: pjxx_k1(:) => null()
    character(len=8), pointer :: ce1c(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    integer, pointer :: pjef_m1(:) => null()
    integer, pointer :: videcal(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    iret = 0
    ces1 = ces1z
    ces2 = ces2z
    base = basez
    corres = correz
    call infniv(ifm, niv)
!
!
!     1- ON TRANSFORME LE CHAM_ELEM_S INITIAL EN CHAM_ELEM_S/ELNO
!        (SI C'EST NECESSAIRE)
!     -------------------------------------------------------------
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    typces = zk8(jce1k-1+3)
    if (typces .eq. 'ELNO') then
        ces1 = ces1z
        ces2 = ces2z
!
    else if (typces.eq.'ELGA') then
!       -- ON NE PEUT PAS ENCORE TRAITER LES CHAMPS ELGA
        iret = 1
        goto 80
!
    else if (typces.eq.'ELEM') then
        ces1 = '&&CESPRJ.CES1'
        ces2 = '&&CESPRJ.CES2'
        call cesces(ces1z, 'ELNO', ' ', ' ', ' ',&
                    'V', ces1)
!
    else
        ASSERT(.false.)
    endif
!
!
!     1- RECUPERATION D'INFORMATIONS DANS CES1 :
!     ------------------------------------------
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
    call jeveuo(ces1//'.CESC', 'L', vk8=ce1c)
    call jeveuo(ces1//'.CESV', 'L', jce1v)
    call jeveuo(ces1//'.CESL', 'L', jce1l)
!
    ma1 = zk8(jce1k-1+1)
    nomgd = zk8(jce1k-1+2)
    ncmpmx = zi(jce1d-1+2)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!
!------------------------------------------------------------------
!     2- RECUPERATION DES OBJETS ET INFORMATIONS DE CORRES :
!     ----------------------------------------------------
    call jeveuo(corres//'.PJXX_K1', 'L', vk24=pjxx_k1)
    call jeveuo(corres//'.PJEF_NB', 'L', vi=pjef_nb)
    call jeveuo(corres//'.PJEF_M1', 'L', vi=pjef_m1)
    call jeveuo(corres//'.PJEF_NU', 'L', iaconu)
    call jeveuo(corres//'.PJEF_CF', 'L', vr=pjef_cf)
!
    ma2 = pjxx_k1(2)
!
!------------------------------------------------------------------
!     3- QUELQUES VERIFS :
!     ------------------------
    if (zi(jce1d-1+4) .gt. 1) then
!       -- ON NE PEUT PAS TRAITER LES CHAMPS A SOUS-POINTS
        iret = 1
        goto 80
!
    endif
!
    if (tsca .ne. 'R' .and. tsca .ne. 'C') then
!       -- ON NE TRAITE POUR L'INSTANT QUE LES CHAMPS REELS
        iret = 1
        goto 80
!
    endif
!     TEST SUR IDENTITE DES 2 MAILLAGES
    ASSERT(pjxx_k1(1).eq.ma1)
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) then
        call utmess('F', 'CALCULEL_67', sk=nomgd)
    endif
!
!
!------------------------------------------------------------------
!     4- ALLOCATION DE CES2 (ELNO):
!     -----------------------------
    call detrsd('CHAM_ELEM_S', ces2)
    call cescre(base, ces2, 'ELNO', ma2, nomgd,&
                ncmpmx, ce1c, [0], [-1], [-ncmpmx])
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
    nbmam2 = zi(jce2d-1+1)
!
!
!------------------------------------------------------------------
!     5- CALCUL DES VALEURS DE CES2 :
!     -------------------------------
!
!       -- IL FAUT FABRIQUER UN OBJET TEMPORAIRE POUR UTILISER CORRES
!          DANS UNE OPTIQUE "CHAM_ELEM" : UNE ESPECE DE POINTEUR DE
!          LONGUEUR CUMULEE SUR LES OBJETS .PJEF_NU ET .PJEF_CF
    call dismoi('NB_NO_MAILLA', ma2, 'MAILLAGE', repi=nbno2)
    AS_ALLOCATE(vi=videcal, size=nbno2)
    idecal = 0
!
    do ino2 = 1, nbno2
        nbno1 = pjef_nb(ino2)
        videcal(ino2) = idecal
        idecal = idecal + nbno1
    end do
    call jeveuo(ma2//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(ma2//'.CONNEX', 'LONCUM'), 'L', ilcnx2)
!
!
    do ima2 = 1, nbmam2
        nbno2 = zi(jce2d-1+5+4* (ima2-1)+1)
        do ino2 = 1, nbno2
            nuno2 = connex(1+zi(ilcnx2-1+ima2)-2+ino2)
            nbno1 = pjef_nb(nuno2)
            ima1 = pjef_m1(nuno2)
            idecal = videcal(nuno2)
            do icmp = 1, ncmpmx
! ================================================================
! --- ON NE PROJETTE UNE CMP QUE SI ELLE EST PORTEE
!     PAR TOUS LES NOEUDS DE LA MAILLE SOUS-JACENTE
!  EN PRINCIPE, C'EST TOUJOURS LE CAS POUR LES CHAM_ELEM
! ================================================================
                ico = 0
                do ino1 = 1, nbno1
                    call cesexi('C', jce1d, jce1l, ima1, ino1,&
                                1, icmp, iad1)
                    coef1 = pjef_cf(1+idecal-1+ino1)
                    if (iad1 .gt. 0) ico = ico + 1
                end do
                if (ico .eq. 0) goto 50
                if (ico .lt. nbno1) goto 50
!
                call cesexi('S', jce2d, jce2l, ima2, ino2,&
                            1, icmp, iad2)
                ASSERT(iad2.lt.0)
                zl(jce2l-1-iad2) = .true.
!
                if (tsca .eq. 'R') then
                    v2 = 0.d0
                    do ino1 = 1, nbno1
                        coef1 = pjef_cf(1+idecal-1+ino1)
                        call cesexi('C', jce1d, jce1l, ima1, ino1,&
                                    1, icmp, iad1)
                        ASSERT(iad1.gt.0)
                        v1 = zr(jce1v-1+iad1)
                        v2 = v2 + coef1*v1
                    end do
                    zr(jce2v-1-iad2) = v2
!
                else if (tsca.eq.'C') then
                    v2c = dcmplx(0.d0,0.d0)
                    do ino1 = 1, nbno1
                        coef1 = pjef_cf(1+idecal-1+ino1)
                        call cesexi('C', jce1d, jce1l, ima1, ino1,&
                                    1, icmp, iad1)
                        ASSERT(iad1.gt.0)
                        v1c = zc(jce1v-1+iad1)
                        v2c = v2c + coef1*v1c
                    end do
                    zc(jce2v-1-iad2) = v2c
                endif
!
 50             continue
            end do
        end do
    end do
!
!
!     -- VERIFICATION DE LA QUALITE DE CES2:
    if (niv .gt. 1) call cesver(ces2)
!
!
!     -- ON TRANSFORME LE CHAM_ELEM_S/ELNO EN ELEM SI NECESSAIRE:
    if (typces .eq. 'ELEM') then
        call cesces(ces2, 'ELEM', ' ', ' ', ' ',&
                    base, ces2z)
    endif
!
!
!
!     -- MENAGE :
    if (typces .eq. 'ELEM') then
        call detrsd('CHAM_ELEM_S', ces1)
        call detrsd('CHAM_ELEM_S', ces2)
    endif
    AS_DEALLOCATE(vi=videcal)
!
 80 continue
    call jedema()
end subroutine
