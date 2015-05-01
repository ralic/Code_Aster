subroutine pebpct(ligrel, nbma, lma, cham, nomcmp,&
                  dim, bfix, borne, norme, seuil,&
                  lseuil, borpct, voltot, carele, cespoi)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/r8miem.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/chpond.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: dim, nbma, bfix
    real(kind=8) :: borpct(dim), borne(2), seuil, voltot
    character(len=*) :: ligrel
    character(len=8) :: nomcmp, norme, carele
    character(len=19) :: cham,cespoi
    character(len=24) :: lma
    aster_logical :: lseuil
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR :  POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR : "VOLUMOGRAMME"
!
!     BUT :  DETERMINE LES BORNES INF ET SUP DE CHAQUE INTERVALLE
!            AINSI QUE LA DISTRIBUTION VOLUMIQUE OU SURFACIQUE
!            DE LA COMPOSANTE POUR CHAQUE INTERVALLE
!
!    ARGUMENTS:
!
!     IN :
!          LIGREL  =  NOM DU LIGREL
!          NBMA    =  NOMBRE DE MAILLES A CONSIDERER
!          LMA     =  NOM JEVEUX DES NUMEROS DE MAILLES A CONSIDERER
!          CHAM    =  NOM DU CHAMP
!          NOMCMP  =  NOM DE LA COMPOSANTE
!          DIM     =  3 * NOMBRE D'INTERVALLES
!          BFIX    =  CALCUL DU MIN/MAX POUR DEFINIR LES BORNES
!                     (0=OUI, 1=BORNES FOURNIES PAR L UTILISATEUR)
!          BORNE   =  BORNE MIN/MAX DE LA PLAGE
!                     - SI FIXEE PAR L UTILISATEUR
!          NORME   =  VOLUME CALCULE RELATIF OU ABSOLU
!          SEUIL   =  SEUIL POUR CALCULER LA DISTRIBUTION
!          LSEUIL  =  .TRUE. : L'UTILISATEUR A DONNE SEUIL
!                     .FALSE. : L'UTILISATEUR A DONNE NB_INTERV
!     IN/OUT :
!          BORPCT  =  TABLEAU RESULTAT
!                     ON Y STOCKE RESPECTIVEMENT POUR CHAQUE
!                     INTERVALLE: (1) : LA BORNE INF
!                                 (2) : LA BORNE SUP
!                                 (3) : LE POURCENTAGE VOLUMIQUE
!          VOLTOT  =  VOLUME TOTAL CONCERNE PAR LE FILTRE
!
!     ------------------------------------------------------------------
!
    integer :: nbmat, i, nbintv
    integer :: jcesl, jcesd, jcesk, jpoil, jpoid, jval
    integer :: jvol, ispt, nbpspt
    integer :: ima, nbsp, nbpt, iad, ipt, j, jnuma, nbptmx, k
    real(kind=8) :: volpt, pas, p0, valmin, valmax, pdiv
    integer :: ncmpm, nucmp, nbval
    character(len=4) :: tych, non
    character(len=8) :: noma
    character(len=19) ::  cesout, chams
    character(len=24) :: tabval, tabvol
    aster_logical :: first
    character(len=8), pointer :: cesc(:) => null()
    real(kind=8), pointer :: pdsm(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    real(kind=8), pointer :: poiv(:) => null()
    integer, pointer :: repe(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmat)
    call jeveuo(ligrel//'.REPE', 'L', vi=repe)
!
! --- CALCULS DES VALEURS ET VOLUMES POUR CHAQUE POINT DE CHAQUE MAILLE
!     ----------------------------------------------------------------
    tabval='&&PEBPCT_VAL_CMP_MAIL'
    tabvol='&&PEBPCT_VOLUME_MAIL'
    cesout='&&PEBPCT_CHAMS_POND'
    chams='&&PEBPCT.CHAM_EL_S'


!   -- passage au champ simple
!   ----------------------------
    call celces(cham, 'V', chams)
    call jeveuo(chams//'.CESV', 'L', vr=cesv)
    call jeveuo(chams//'.CESL', 'L', jcesl)
    call jeveuo(chams//'.CESD', 'L', jcesd)
    call jeveuo(chams//'.CESC', 'L', vk8=cesc)
    call jeveuo(chams//'.CESK', 'L', jcesk)
    nbptmx=zi(jcesd+2)
    nbpspt=zi(jcesd+3)


!   -- determination des poids des points de gauss
!   -----------------------------------------------
    non='NON'
    call dismoi('TYPE_CHAMP', cham, 'CHAMP', repk=tych)
    call chpond(tych, non, cham, cesout, cespoi,&
                ligrel, carele)
    call jeveuo(cespoi//'.CESV', 'L', vr=poiv)
    call jeveuo(cespoi//'.CESL', 'L', jpoil)
    call jeveuo(cespoi//'.CESD', 'L', jpoid)
    if (tych .ne. 'ELGA') call jeveuo(cespoi//'.PDSM', 'L', vr=pdsm)
!
!   -- creation des tableaux recensant les valeurs de la composante
!      (val) et le volume*poids (vol) associe
    call wkvect(tabval, 'V V R', nbma*nbptmx*nbpspt, jval)
    call wkvect(tabvol, 'V V R', nbma*nbptmx*nbpspt, jvol)
!
    call jelira(chams//'.CESC', 'LONMAX', ncmpm)
    nucmp=indik8(cesc,nomcmp,1,ncmpm)

    if (nucmp.le.0) then
        call utmess('F', 'CHAMPS_3', sk=nomcmp)
    endif
!
!   -- mailles a considerer :
    call jeveuo(lma, 'L', jnuma)
!
    voltot=0.d0
    first=.true.
    k=0


!   -- on remplit les tableaux val et vol
!   --------------------------------------
    do i = 1, nbma
!
        ima=zi(jnuma+i-1)
        nbpt=zi(jcesd-1+5+4*(ima-1)+1)
        nbsp=zi(jcesd-1+5+4*(ima-1)+2)
!
        do ipt = 1, nbpt
          do ispt = 1, nbsp
!
            call cesexi('C', jcesd, jcesl, ima, ipt,&
                        ispt, nucmp, iad)
!
            if ((iad.gt.0) .and. (bfix.eq.0)) then
!
                k=k+1
                zr(jval+k-1)=cesv(iad)
!
                if (tych .eq. 'ELGA') then
                    call cesexi('C', jpoid, jpoil, ima, ipt,&
                                ispt, 1, iad)
                    ASSERT(iad.gt.0)
                    volpt=poiv(iad)
                else if (tych.eq.'ELEM') then
                    ASSERT(nbpt.eq.1)
                    volpt=pdsm(ima)
                else if (tych.eq.'ELNO') then
                    ASSERT(nbpt.ge.1)
                    volpt=pdsm((ima-1)*nbsp+ispt)/nbpt
                endif
!
                zr(jvol+k-1)=volpt
                voltot=voltot+volpt
!
                if (first) then
                    valmin=zr(jval+k-1)
                    valmax=zr(jval+k-1)
                    first=.false.
                else
                    if (zr(jval+k-1) .le. valmin) valmin=zr(jval+k-1)
                    if (zr(jval+k-1) .ge. valmax) valmax=zr(jval+k-1)
                endif
!
            else if ((iad.gt.0).and.(bfix.eq.1)) then
!
                if ((cesv(iad).ge.borne(1)) .and. (cesv(iad).le.borne(2))) then
                    k=k+1
                    zr(jval+k-1)=cesv(iad)
!
                    if (tych .eq. 'ELGA') then
                        call cesexi('C', jpoid, jpoil, ima, ipt,&
                                    ispt, 1, iad)
                        ASSERT(iad.gt.0)
                        volpt=poiv(iad)
                    else if (tych.eq.'ELEM') then
                        ASSERT(nbpt.eq.1)
                        volpt=pdsm(ima)
                    else if (tych.eq.'ELNO') then
                        ASSERT(nbpt.ge.1)
                        volpt=pdsm((ima-1)*nbsp+ispt)/nbpt
                    endif
!
                    zr(jvol+k-1)=volpt
                    voltot=voltot+volpt
!
                endif
!
            endif
!
          end do
        end do
!
    end do
!
!     NOMBRE DE VALEURS STOCKEES
    nbval=k
!
!     NOMBRE D'INTERVALLES
    nbintv=dim/3
!
!
! --- DETERMINATION DES INTERVALLES
!
!     CAS BORNES FIXEES PAR L UTILISATEUR : BORNE(1)-BORNE(2)
!     ON REMPLACE LES EXTREMA CALCULES VALMIN-VALMAX
!     PAR LES BORNES FOURNIES BORNE(1)-BORNE(2)
    if (bfix .eq. 1) then
        valmin=borne(1)
        valmax=borne(2)
!     CAS OU AUCUNE VALEUR N'A ETE TROUVEE
    else if (abs(valmin-valmax).le.r8miem()) then
        do i = 1, nbintv
            borpct(3*(i-1)+3)=100.d0/nbintv
        end do
        goto 100
    endif
!
    p0=valmin
    if (lseuil) then
        ASSERT(nbintv .eq. 2)
        pas = seuil
        do i = 1, nbintv
            borpct(3*(i-1)+1)=p0
            borpct(3*(i-1)+2)=pas
            p0=seuil
            pas=valmax
        end do
    else
        pas=(valmax-valmin)/nbintv
        do i = 1, nbintv
            borpct(3*(i-1)+1)=p0
            borpct(3*(i-1)+2)=p0+pas
            p0=p0+pas
        end do
    endif
!
! --- AJOUT DES VOLUMES DANS 'BORPCT' EN FONCTION DES VALEURS
!     DE LA COMPOSANTE
    do j = 1, nbval
        if (zr(jval+j-1) .lt. borpct(2)) then
            borpct(3)=borpct(3)+zr(jvol+j-1)
        endif
    end do
    do i = 2, nbintv-1
        do j = 1, nbval
            if (zr(jval+j-1) .lt. borpct(3*(i-1)+2) .and. zr(jval+j-1) .ge.&
                borpct(3*(i-1)+1)) then
                borpct(3*(i-1)+3)=borpct(3*(i-1)+3)+zr(jvol+j-1)
            endif
        end do
    end do
    do j = 1, nbval
        if (zr(jval+j-1) .ge. borpct(3*(nbintv-1)+1)) then
            borpct(3*(nbintv-1)+3)=borpct(3*(nbintv-1)+3)+zr(jvol+j-1)
        endif
    end do
!
    if (norme(1:7) .eq. 'RELATIF') then
        pdiv=voltot
    else
        pdiv=1.d2
    endif
!
    do i = 1, nbintv
        borpct(3*(i-1)+3)=100*borpct(3*(i-1)+3)/pdiv
    end do
!
100 continue
!
    call detrsd('CHAM_ELEM_S', chams)
    call jedetr(tabval)
    call jedetr(tabvol)
!
    call jedema()
!
end subroutine
