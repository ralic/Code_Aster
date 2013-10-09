subroutine memaxm(typmx, champ, nocmp, nbcmp, lcmp,&
                  vr, nbmail, numail)
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8nnem.h"
#include "asterfort/asmpi_comm_vect.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: typmx
    character(len=*) :: champ, nocmp, lcmp(*)
    integer :: nbcmp, nbmail, numail(*)
    real(kind=8) :: vr(*)
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
! BUT :  EXTRAIRE LE "MIN/MAX" DE COMPOSANTES
!        D'UN CHAMP (CHAM_ELEM OU CARTE) SUIVANT LA COMPOSANTE NOCMP
!
! EXEMPLE :
!   CONSIDERONS 2 MAILLES M1 ET M2 AVEC LES VALEURS D'UN CHAMP
!   (DX,DY,DZ):
!           SUR LA MAILLE M1 ---> (5,3,1)
!           SUR LA MAILLE M2 ---> (1,4,3)
!   SI LA COMPOSANTE CRITERE EST NOMCMP='DZ',ET QUE L'UTILISA-
!   TEUR DEMANDE LES VALEURS DES COMPOSANTES DX ET DY DU CHAMP
!   SUR L'ELEMENT OU LE MAX EST ATTEINT,LA FONCTION RETOURNERA:
!           VR=(1,4)
!   SI LA COMPOSANTE CRITERE EST NOMCMP='DZ',ET QUE L'UTILISA-
!   TEUR DEMANDE LA VALEUR DE LA COMPOSANTE DX DU CHAMP SUR
!   L'ELEMENT OU LE MIN EST ATTEINT,LA FONCTION RETOURNERA:
!           VR=5
!   SI LA COMPOSANTE CRITERE EST NOMCMP='DY',ET QUE L'UTILISA-
!   TEUR DEMANDE LA VALEUR DES COMPOSANTES DX,DY ET DZ DU CHAMP
!   SUR L'ELEMENT OU LE MAX EST ATTEINT,LA FONCTION RETOURNERA:
!           VR=(1,4,3)
!
! IN  : TYPMX  :  'MIN'/'MAX'/'MIN_ABS'/'MAX_ABS'
! IN  : CHAMP  :  NOM DU CHAMP A SCRUTER (VALEURS REELLES OU ENTIERES)
! IN  : NOCMP  :  NOM DE LA COMPOSANTE SUR LAQUELLE ON FAIT LE TEST
! IN  : NBCMP  :  NOMBRE DE COMPOSANTES DEMANDEES (=LONGUEUR DE VR)
! IN  : LICMP  :  NOM DES COMPOSANTES DEMANDEES PAR L'UTILISATEUR
! IN  : NBMAIL :  = 0   , COMPARAISON SUR TOUT LE MAILLAGE
!                 SINON , COMPARAISON SUR UNE PARTIE DU MAILLAGE
! IN  : NUMAIL :  NUMEROS DES MAILLES SUR LESQUELLES ON EFFECTUE LES
!                 COMPARAISONS (SI NBMAIL>0)
! OUT : VR     :  VECTEUR CONTENANT LES VALEURS DES COMPOSANTES DU CHAMP
!                 SUR L'ELEMENT (OU NOEUD OU POINT DE GAUSS) OU LE
!                 'MIN'/'MAX' EST ATTEINT
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer ::  iret
    integer :: longt
    character(len=8) :: kmpic, typ1, nomgd, tsca, tych
    integer :: jcesd, jcesl, jcesc, jcesv, nel, iel, nbpt, nbsspt, ncmp
    integer :: ipt, isp, icmp, ncp, iicmp, iadr1, jcesk
    integer :: iadr2, iel1
    real(kind=8) :: valr, vmima
    character(len=19) :: chams, cham19
    integer :: tncomp(nbcmp)
    logical :: copi, lmax, labs, lreel
!     ------------------------------------------------------------------
!
    call jemarq()
!
    cham19=champ
!
!     -- TRANSFORMATION DU CHAMP EN CHAMP SIMPLE :
!     --------------------------------------------
    chams='&&MEMAXM.CES'
    call dismoi('TYPE_CHAMP', cham19, 'CHAMP', repk=tych)
    if (tych(1:2) .eq. 'EL') then
        call celces(cham19, 'V', chams)
    else if (tych.eq.'CART') then
        call carces(cham19, 'ELEM', ' ', 'V', chams,&
                    ' ', iret)
        ASSERT(iret.eq.0)
    else
        ASSERT(.false.)
    endif
    call jelira(chams//'.CESV', 'TYPE', cval=typ1)
    ASSERT(typ1.eq.'R')
!
!
!
    call jeveuo(chams//'.CESD', 'L', jcesd)
    call jeveuo(chams//'.CESL', 'L', jcesl)
    call jeveuo(chams//'.CESC', 'L', jcesc)
    call jeveuo(chams//'.CESK', 'L', jcesk)
    call jeveuo(chams//'.CESV', 'L', jcesv)
!
    nomgd=zk8(jcesk-1+2)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    ASSERT(tsca.eq.'R'.or.tsca.eq.'I')
    lreel=tsca.eq.'R'
!
!
!
!     -- INITIALISATION DE VMIMA :
    ASSERT(typmx(1:3).eq.'MIN'.or.typmx(1:3).eq.'MAX')
    lmax=typmx(1:3).eq.'MAX'
    if (.not.lmax) vmima=+r8maem()
    if (len(typmx) .gt. 3) then
        ASSERT(len(typmx).eq.7)
        ASSERT(typmx(4:7).eq.'_ABS')
        labs=.true.
        if (lmax) vmima=0.d0
    else
        labs=.false.
        if (lmax) vmima=-r8maem()
    endif
!
!
!     INITIALISATION DE TNCOMP CONTENANT LES INDICES
!     DES CMP
!     ----------------------------------
    do 10 icmp = 1, nbcmp
        tncomp(icmp)=0
 10 end do
!
    ncmp=zi(jcesd-1+2)
    do 30 icmp = 1, ncmp
        do 20 iicmp = 1, nbcmp
            if (lcmp(iicmp) .eq. zk8(jcesc-1+icmp)) then
                tncomp(iicmp)=icmp
            endif
 20     continue
 30 end do
!
!
!     COMPARAISON NOCMP AVEC TTES LES
!     AUTRES AFIN DE RECUPERER LE NUM DE LA COMPOSANTE
!     RECUPERE L'INDEX DE LA COMPOSANTE A TESTER DANS LE CHAMP
    ncp=0
    do 40 icmp = 1, ncmp
        if (zk8(jcesc-1+icmp) .eq. nocmp) ncp=icmp
 40 end do
!
!     -- CAS : TOUTES LES MAILLES :
!     -----------------------------
    if (nbmail .le. 0) then
!       NOMBRE D'ELEMENTS DU MAILLAGE
        nel=zi(jcesd-1+1)
!     -- CAS : LISTE DE MAILLES :
!     ---------------------------
    else
        nel=nbmail
    endif
!
!
    do 80 iel = 1, nel
!
        if (nbmail .le. 0) then
            iel1=iel
        else
            iel1=numail(iel)
        endif
!
!       NOMBRE DE PTS ET SSPTS POUR CHAQUE ELEMENT
        nbpt=zi(jcesd-1+5+4*(iel1-1)+1)
        nbsspt=zi(jcesd-1+5+4*(iel1-1)+2)
        ncmp=zi(jcesd-1+5+4*(iel1-1)+3)
!
!
        do 70 ipt = 1, nbpt
            do 60 isp = 1, nbsspt
                call cesexi('C', jcesd, jcesl, iel1, ipt,&
                            isp, ncp, iadr1)
                if (iadr1 .gt. 0) then
                    if (lreel) then
                        valr=zr(jcesv-1+iadr1)
                    else
                        valr=zi(jcesv-1+iadr1)
                    endif
                    if (labs) valr=abs(valr)
                    copi=.false.
                    if ((lmax) .and. (valr.gt.vmima)) copi=.true.
                    if ((.not.lmax) .and. (valr.lt.vmima)) copi=.true.
                    if (copi) then
                        vmima=valr
                        do 50 iicmp = 1, nbcmp
                            call cesexi('C', jcesd, jcesl, iel1, ipt,&
                                        isp, tncomp(iicmp), iadr2)
                            if (iadr2 .eq. 0) then
                                vr(iicmp)=r8nnem()
                            else
                                vr(iicmp)=zr(jcesv-1+iadr2)
                            endif
 50                     continue
                    endif
                endif
 60         continue
 70     continue
 80 end do
!
!
    call detrsd('CHAMP', chams)
!
!     -- IL FAUT PARFOIS COMMUNIQUER LE RESULTAT ENTRE LES PROCS :
    call dismoi('MPI_COMPLET', champ, 'CHAMP', repk=kmpic)
    if (kmpic .eq. 'NON') then
        if (lmax) then
            call asmpi_comm_vect('MPI_MAX', 'R', nbval=longt, vr=vr)
        else
            call asmpi_comm_vect('MPI_MIN', 'R', nbval=longt, vr=vr)
        endif
    endif
!
    call jedema()
end subroutine
