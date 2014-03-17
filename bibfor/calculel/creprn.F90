subroutine creprn(ligrez, molocz, basez, prnmz, prnsz)
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
!
! aslint: disable=
    implicit none
!-----------------------------------------------------------------
!  BUT : CREATION :
!      .DU VECTEUR PRNM DES ENTIERS CODES DECRIVANT
!       LA NATURE DES DDLS DES NOEUDS PHYSIQUES DU LIGREL.
!
!      .DU VECTEUR PRNS DES ENTIERS CODES DECRIVANT
!       LES NOEUDS LAGRANGE DU LIGREL
!       (S'IL Y EN A)
!-----------------------------------------------------------------
!   ARGUMENT        E/S  TYPE         ROLE
!    LIGREZ          IN    K*     NOM DU LIGREL
!    MOLOCZ          IN    K*   / NOM DU MODE_LOCAL PERMETTANT
!                                 DE CHOISIR LES DDLS.
!                               / ' '
!    BASEZ           IN    K*     NOM DE LA BASE
!    PRNMZ      IN/JXOUT    K24   NOM DU VECTEUR DES ENTIERS CODES
!                                 DECRIVANT LA NATURE DES DDLS DES
!                                 NOEUDS PHYSIQUES
!   (CE VECTEUR EST TOUJOURS CREE)
!    PRNSZ      IN/JXOUT    K24   NOM DU VECTEUR DES ENTIERS CODES
!                                 DECRIVANT LES NOEUDS LAGRANGE
!   (CE VECTEUR N'EST CREE QUI SI LIGREL CONTIENT DES NOEUDS TARDIFS)
!-----------------------------------------------------------------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/entcod.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/nbno.h"
#include "asterfort/typele.h"
#include "asterfort/wkvect.h"
!
!
! -----  VARIABLES LOCALES
    character(len=*) :: ligrez, molocz, basez, prnmz, prnsz
    integer :: gd, i,   iamaco, iamail, iamsco, jmoloc, iancmp
    integer ::  iaprnm, iaprno, iaprns,  icmp
    integer :: icodla, iec, igr, illiel, ilmaco, ilmsco
    integer :: ima, imode, ino, inold, iret, ite, j, k, l, lgncmp, nbnm
    integer :: nbnoms, nbsma, nbssa, nec, nel, nl, nm, nnoe, numa, nunoel
    integer :: admodl, lcmodl
    integer :: lshift
    character(len=1) :: base
    character(len=8) :: noma, nomgd, exiel, nomacr, moloc
    character(len=16) :: nomte
    character(len=14) :: num2
    character(len=16) :: phenom
    character(len=19) :: ligrel
    character(len=24) :: prnm, prns
    integer, pointer :: liel(:) => null()
    character(len=8), pointer :: vnomacr(:) => null()
    integer, pointer :: sssa(:) => null()
    integer, pointer :: conx(:) => null()
!
! -----  FONCTIONS FORMULES
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
#define numail(igr,iel) liel(zi(illiel+igr-1)+iel-1)
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
#define numglm(ima,ino) zi(iamaco-1+zi(ilmaco+ima-1)+ino-1)
!     NUMGLS(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE SUPPLEMENTAIRE DU LIGREL
#define numgls(ima,ino) zi(iamsco-1+zi(ilmsco+ima-1)+ino-1)
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    base = basez
    moloc = molocz
    ligrel = ligrez
    prnm = prnmz
    prns = prnsz
    nm = 0
    nl = 0
    nbnoms = 0
    nbsma = 0
    nbssa = 0
!
!
    call dismoi('EXI_ELEM', ligrel, 'LIGREL', repk=exiel)
    call dismoi('NB_SS_ACTI', ligrel, 'LIGREL', repi=nbssa)
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=noma)
!
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nm)
    call dismoi('NB_NL_MAILLA', noma, 'MAILLAGE', repi=nl)
    call dismoi('NB_SM_MAILLA', noma, 'MAILLAGE', repi=nbsma)
!
    call jeveuo(jexatr('&CATA.TE.MODELOC', 'LONCUM'), 'L', lcmodl)
    call jeveuo(jexnum('&CATA.TE.MODELOC', 1), 'L', admodl)
!
    if (moloc .eq. ' ') then
        call dismoi('PHENOMENE', ligrel, 'LIGREL', repk=phenom)
        call dismoi('NOM_MOLOC', phenom, 'PHENOMENE', repk=moloc)
    endif
!
!
!     -- DETERMINATION DE LA GRANDEUR NOMGD A PARTIR DE MOLOC :
!     ---------------------------------------------------------
    if (exiel(1:3) .eq. 'OUI') then
        do igr = 1, nbgrel(ligrel)
            ite = typele(ligrel,igr)
            call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
            call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//moloc), imode)
            if (imode .gt. 0) then
                call jeveuo(jexnum('&CATA.TE.MODELOC', imode), 'L', jmoloc)
                call jenuno(jexnum('&CATA.GD.NOMGD', zi(jmoloc-1+2)), nomgd)
                goto 20
            endif
        end do
!       -- IL PEUT ARRIVER QUE NBGREL=0. ON S'EN SORT AVEC MOLOC :
        if (moloc .eq. 'DDL_MECA') then
            nomgd='DEPL_R'
        else if (moloc.eq.'DDL_THER') then
            nomgd='TEMP_R'
        else
            ASSERT(.false.)
        endif
 20     continue
!
    else
!       -- SI IL N'Y A PAS D'ELEMENTS FINIS
!          ON EST EN SOUS-STRUCTURATION STATIQUE => MECANIQUE.
        call dismoi('NOM_GD', 'MECANIQUE', 'PHENOMENE', repk=nomgd)
    endif
!
!
!     -- CALCUL DE GD ET NEC :
!     ---------------------------------------------------------
    call dismoi('NUM_GD_SI', nomgd, 'GRANDEUR', repi=gd)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
!
!
    call jeexin(noma//'.CONNEX', iret)
    if (iret .gt. 0) then
        call jeveuo(noma//'.CONNEX', 'L', iamaco)
        call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilmaco)
    else
        iamaco=1
        ilmaco=1
    endif
!
    call jeexin(ligrel(1:19)//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(ligrel(1:19)//'.NEMA', 'L', iamsco)
        call jeveuo(jexatr(ligrel(1:19)//'.NEMA', 'LONCUM'), 'L', ilmsco)
    else
        iamsco=1
        ilmsco=1
    endif
!
!
! --- ALLOCATION DE PRNM :
!     ------------------
    call jeexin(prnm, iret)
    if (iret .eq. 0) then
        call wkvect(prnm, base//' V I', (nm+nl)*nec, iaprnm)
    else
        call jeveuo(prnm, 'L', iaprnm)
    endif


! - allocation de prns (pour un ligrel contenant des noeuds tardifs):
!   ----------------------------------------------------------------
    call dismoi('NB_NO_SUP', ligrel, 'LIGREL', repi=nbnoms)
    call jeexin(prns, iret)
    if (iret .eq. 0) then
        if (nbnoms .gt. 0) call wkvect(prns, base//' V I', nbnoms*nec, iaprns)
    else
        call jeveuo(prns, 'L', iaprns)
    endif


! - traitement des elements finis classiques :
!   ----------------------------------------
    if (exiel(1:3) .eq. 'NON') goto 90
    call jeveuo(ligrel(1:19)//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel(1:19)//'.LIEL', 'LONCUM'), 'L', illiel)

    do igr = 1, nbgrel(ligrel)
!
! ---   calcul de imode (mode_local) :
!       ------------------------------
        ite = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
        call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//moloc), imode)


        if (imode .gt. 0) then
            nnoe = nbno(imode)
            nel = nbelem(ligrel,igr)
            do j = 1, nel
                numa = numail(igr,j)
                if (numa .gt. 0) then

!                   -- il s'agit d'une maille physique du maillage :
!                   ------------------------------------------------
                    do k = 1, nnoe
                        nunoel = numglm(numa,k)
                        do l = 1, nec
                            iec = entcod(admodl,lcmodl,nec,imode,k,l)
                            zi(iaprnm-1+nec* (nunoel-1)+ l) = ior(&
                                                              zi( iaprnm-1+nec* ( nunoel-1)+l ),&
                                                              iec&
                                                              )
                        end do
                    end do
                else

!                   -- il s'agit d'une maille tardive :
!                   -----------------------------------
                    numa = -numa
                    do k = 1, nnoe
                        nunoel = numgls(numa,k)
                        do l = 1, nec
                            iec = entcod(admodl,lcmodl,nec,imode,k,l)
                            if (nunoel .gt. 0) then
                                zi(iaprnm-1+nec* (nunoel-1)+l) =&
                                ior(zi(iaprnm-1+nec* (nunoel-1)+l),&
                                iec)
                            else
                                zi(iaprns-1+nec* (-nunoel-1)+ l) =&
                                ior(zi(iaprns-1+nec* (-nunoel-1)+l),&
                                iec)
                            endif
                        end do
                    end do
                endif
            end do
        endif
    end do

 90 continue


! --- BOUCLE SUR LES SUPERELEMENTS :
!     ----------------------------
    if (nbssa .gt. 0) then
!
        call jeveuo(ligrel//'.SSSA', 'L', vi=sssa)
!
! ---   le seul ddl porte par un noeud de lagrange est 'lagr' :
!       -------------------------------------------------------
        call jeveuo(noma//'.NOMACR', 'L', vk8=vnomacr)
!
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iancmp)
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', lgncmp)
        icmp = indik8(zk8(iancmp),'LAGR',1,lgncmp)
! on ne trouve pas la composante "LAGR" dans la grandeur
        ASSERT(icmp.ne.0)
! il est imprévu d avoir la composante "LAGR" au delà de 30
        ASSERT(icmp.le.30)
!
        icodla = lshift(1,icmp)
!
        do ima = 1, nbsma
            nomacr = vnomacr(ima)
            call dismoi('NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', repk=num2)
            call jeveuo(nomacr//'.CONX', 'L', vi=conx)
            call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', iaprno)
            if (sssa(ima) .eq. 1) then
                call jeveuo(jexnum(noma//'.SUPMAIL', ima), 'L', iamail)
                call jelira(jexnum(noma//'.SUPMAIL', ima), 'LONMAX', nbnm)
!
                do i = 1, nbnm
                    ino = zi(iamail-1+i)
                    inold = conx(3* (i-1)+2)
                    if (ino .gt. nm) then
!
! ---                   CAS D'UN NOEUD DE LAGRANGE :
!                       --------------------------
                        zi(iaprnm-1+nec* (ino-1)+1) = ior(zi(iaprnm-1+ nec* (ino- 1)+1), icodla)

                    else if (inold.gt.0) then
!
! ---                   CAS D'UN NOEUD PHYSIQUE DU MAILLAGE :
!                       -----------------------------------
                        do iec = 1, nec
                            zi(iaprnm-1+nec* (ino-1)+iec) = ior(&
                                        zi(iaprnm-1+ nec* (ino-1)+iec),&
                                        zi(iaprno-1+ ( nec+2)* (inold-1)+2+ iec))
                        end do
                    else
! on traite un super-élément  et le noeud courant n'est ni un noeud Lagrange,
! ni un noeud physique du maillage.
                        ASSERT(.false.)
                    endif
                end do
            endif
        end do
    endif
!
    call jedema()
!
end subroutine
