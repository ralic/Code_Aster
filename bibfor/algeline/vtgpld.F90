subroutine vtgpld(cumul, geomiz, alpha, deplaz, base,&
                  geomfz)
!
! ----------------------------------------------------------------------
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
!
    character(len=*) :: geomiz, deplaz, geomfz
    character(len=1) :: base
    real(kind=8) :: alpha
    character(len=4) :: cumul
!
! ----------------------------------------------------------------------
!
! REACTUALISATION D'UN CHAMP GEOMETRIQUE (GEOM_R) AVEC UN CHAMP SUR
! LA GRANDEUR DEPL_R
!
! ----------------------------------------------------------------------
!
! ON FAIT:
!   GEOMF = GEOMI + ALPHA * DEPLA SI CUMUL = 'CUMU'
!   GEOMF = ALPHA * DEPLA         SI CUMUL = 'ZERO'
!
! ON SE SERT UNIQIUEMENT DES COMPOSANTES DX, DY, DZ SUR LE CHAMP
! DE DEPLACEMENT
! SI SUR CERTAINS NOEUDS, ON NE TROUVE PAS DE DEPLACEMENT,
! ON LES LAISSE INCHANGES
!
! IN  CUMUL : 'ZERO' OU 'CUMU'
! IN  GEOMI : CHAM_NO(GEOM_R) - CHAMP DE GEOMETRIE A ACTUALISER.
! IN  ALPHA : COEFFICIENT MULTIPLICATEUR DE DEPLA
! IN  DEPLA : CHAM_NO(DEPL_R) - CHAMP DE DEPLACEMENT A AJOUTER.
! IN  BASE  : BASE SUR LAQUELLE DOIT ETRE CREE GEOMF
! OUT GEOMF : CHAM_NO(GEOM_R) : CHAMP DE GEOMETRIE ACTUALISE.
!                 (CE CHAMP EST DETRUIT S'IL EXISTE DEJA)
!
!
!
!
    integer :: iad, ibid, icompt, igd, ival, ldim
    integer :: nbno, ncmp, ncmpmx, nec, num
    integer ::  iaprno
    integer :: icmp, ino
    real(kind=8) :: rdepla
    character(len=8) :: noma, nomgd, ktype
    character(len=19) :: geomi, depla, geomf
    character(len=24) :: nomnu
    real(kind=8), pointer :: vald(:) => null()
    real(kind=8), pointer :: valf(:) => null()
    real(kind=8), pointer :: vali(:) => null()
    character(len=24), pointer :: refe(:) => null()
    integer, pointer :: desc(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    geomi = geomiz
    geomf = geomfz
    depla = deplaz
!
! --- CONFORMITE DES GRANDEURS
!
    call dismoi('NOM_GD', geomi, 'CHAM_NO', repk=nomgd)
    ASSERT(nomgd(1:6).eq.'GEOM_R')
    call dismoi('NOM_GD', depla, 'CHAM_NO', repk=nomgd)
    ASSERT(nomgd(1:6).eq.'DEPL_R')
    call jelira(depla//'.VALE', 'TYPE', cval=ktype)
    ASSERT(ktype(1:1).eq.'R')
!
! --- ON RECOPIE BESTIALEMENT LE CHAMP POUR CREER LE NOUVEAU
!
    call copisd('CHAMP_GD', base, geomi, geomf)
!
! --- INFORMATIONS SUR LE MAILLAGE
!
    call dismoi('NOM_MAILLA', geomi, 'CHAM_NO', repk=noma)
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbno)
!
    call jelira(geomi//'.VALE', 'LONMAX', ldim)
    ASSERT(ldim/3.eq.nbno)
!
! --- ACCES AUX CHAMPS
!
    call jeveuo(geomi//'.VALE', 'L', vr=vali)
    call jeveuo(geomf//'.VALE', 'E', vr=valf)
    call jeveuo(depla//'.REFE', 'L', vk24=refe)
    call jeveuo(depla//'.VALE', 'L', vr=vald)
    call jeveuo(depla//'.DESC', 'L', vi=desc)
!
! --- INFORMATIONS SUR LE CHAMP DE DEPLACEMENT
!
    nomnu = refe(2)
    igd = desc(1)
    num = desc(2)
    nec = nbec(igd)
    ASSERT(nec.le.10)
    call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', igd), 'L', iad)
    ASSERT(zk8(iad-1+1).eq.'DX')
    ASSERT(zk8(iad-1+2).eq.'DY')
    ASSERT(zk8(iad-1+3).eq.'DZ')
!
! --- SI LE CHAMP EST A REPRESENTATION CONSTANTE
!
    if (num .lt. 0) then
        do ino = 1, nbno
            do icmp = 1, 3
                rdepla = vald(3*(ino-1)+icmp)
                if (cumul .eq. 'CUMU') then
                    valf(3*(ino-1)+icmp) = vali(3*(ino- 1)+icmp )+alpha*rdepla
                else if (cumul.eq.'ZERO') then
                    valf(3*(ino-1)+icmp) = alpha*rdepla
                else
                    ASSERT(.false.)
                endif
            end do
        end do
    else
!
! --- ON RECUPERE CE QUI CONCERNE LES NOEUDS DU MAILLAGE
!
        call jelira(jexnum(nomnu(1:19)//'.PRNO', 1), 'LONMAX', ibid)
        ASSERT(ibid.ne.0)
        call jeveuo(jexnum(nomnu(1:19)//'.PRNO', 1), 'L', iaprno)
        do ino = 1, nbno
            ival = zi(iaprno-1+(ino-1)*(nec+2)+1)
            ncmp = zi(iaprno-1+(ino-1)*(nec+2)+2)
            if (ncmp .ne. 0) then
                icompt = 0
                do icmp = 1, 3
                    if (exisdg(zi(iaprno-1+(ino-1)*(nec+2)+3),icmp)) then
                        icompt = icompt + 1
                        rdepla = vald(ival-1+icompt)
                        if (cumul .eq. 'CUMU') then
                            valf(3*(ino-1)+icmp)= vali(&
                            3*(ino-1)+icmp)+alpha*rdepla
                        else if (cumul.eq.'ZERO') then
                            valf(3*(ino-1)+icmp)= alpha*rdepla
                        else
                            ASSERT(.false.)
                        endif
                    endif
                end do
            endif
        end do
    endif
!
    call jedema()
end subroutine
