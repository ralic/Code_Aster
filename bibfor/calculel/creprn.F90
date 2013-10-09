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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
! -----  VARIABLES LOCALES
    character(len=*) :: ligrez, molocz, basez, prnmz, prnsz
    integer :: gd, i, iaconx, ialiel, iamaco, iamail, iamsco, jmoloc, iancmp
    integer :: ianmcr, iaprnm, iaprno, iaprns, iasssa, icmp
    integer :: icodla, iec, igr, illiel, ilmaco, ilmsco
    integer :: ima, imode, ino, inold, iret, ite, j, k, l, lgncmp, nbnm
    integer :: nbnoms, nbsma, nbssa, nec, nel, nl, nm, nnoe, numa, nunoel
    integer :: admodl, lcmodl
    integer :: lshift
    character(len=1) :: base
    character(len=8) ::  noma, nomgd, exiel, nomacr, moloc
    character(len=16) :: nomte
    character(len=14) :: num2
    character(len=16) :: phenom
    character(len=19) :: ligrel
    character(len=24) :: prnm, prns
!
! -----  FONCTIONS FORMULES
!     NUMAIL(IGR,IEL)=NUMERO DE LA MAILLE ASSOCIEE A L'ELEMENT IEL
#define numail(igr,iel) zi(ialiel-1+zi(illiel+igr-1)+iel-1)
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
    ASSERT(ligrel.ne.'&MAILLA')
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
        do 10 igr = 1, nbgrel(ligrel)
            ite = typele(ligrel,igr)
            call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
            call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//moloc), imode)
            if (imode .gt. 0) then
                call jeveuo(jexnum('&CATA.TE.MODELOC', imode), 'L', jmoloc)
                call jenuno(jexnum('&CATA.GD.NOMGD', zi(jmoloc-1+2)), nomgd)
                goto 20
            endif
 10     continue
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
    endif
!
    call jeexin(ligrel(1:19)//'.NEMA', iret)
    if (iret .gt. 0) then
        call jeveuo(ligrel(1:19)//'.NEMA', 'L', iamsco)
        call jeveuo(jexatr(ligrel(1:19)//'.NEMA', 'LONCUM'), 'L', ilmsco)
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
!
!
! --- ALLOCATION DE PRNS (POUR UN LIGREL CONTENANT DES NOEUDS TARDIFS):
!     ----------------------------------------------------------------
    if (ligrel .ne. '&MAILLA') then
        call dismoi('NB_NO_SUP', ligrel, 'LIGREL', repi=nbnoms)
    endif
    call jeexin(prns, iret)
    if (iret .eq. 0) then
        if (nbnoms .gt. 0) call wkvect(prns, base//' V I', nbnoms*nec, iaprns)
    else
        call jeveuo(prns, 'L', iaprns)
    endif
!
!
! --- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES :
!     ----------------------------------------
    if (exiel(1:3) .eq. 'NON') goto 90
    call jeveuo(ligrel(1:19)//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(ligrel(1:19)//'.LIEL', 'LONCUM'), 'L', illiel)
!
    do 80 igr = 1, nbgrel(ligrel)
!
! ---   CALCUL DE IMODE (MODE_LOCAL) :
!       ------------------------------
        ite = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
        call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//moloc), imode)
!
!
        if (imode .gt. 0) then
            nnoe = nbno(imode)
            nel = nbelem(ligrel,igr)
            do 70 j = 1, nel
                numa = numail(igr,j)
                if (numa .gt. 0) then
!
! ---          IL S'AGIT D'UNE MAILLE PHYSIQUE DU MAILLAGE :
!              -------------------------------------------
                    do 40 k = 1, nnoe
                        nunoel = numglm(numa,k)
                        do 30 l = 1, nec
                            iec = entcod(admodl,lcmodl,nec,imode,k,l)
                            zi(iaprnm-1+nec* (nunoel-1)+ l) = ior(&
                                                              zi( iaprnm-1+nec* ( nunoel-1)+l ),&
                                                              iec&
                                                              )
 30                     continue
 40                 continue
                else
!
! ---          IL S'AGIT D'UNE MAILLE TARDIVE :
!              ------------------------------
                    numa = -numa
                    do 60 k = 1, nnoe
                        nunoel = numgls(numa,k)
                        do 50 l = 1, nec
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
 50                     continue
 60                 continue
                endif
 70         continue
        endif
 80 end do
!
 90 continue
!
!
! --- BOUCLE SUR LES SUPERELEMENTS :
!     ----------------------------
    if (nbssa .gt. 0) then
!
        call jeveuo(ligrel//'.SSSA', 'L', iasssa)
!
! ---   LE SEUL DDL PORTE PAR UN NOEUD DE LAGRANGE EST 'LAGR' :
!       ----------------------------------------------------
        call jeveuo(noma//'.NOMACR', 'L', ianmcr)
!
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iancmp)
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', lgncmp)
        icmp = indik8(zk8(iancmp),'LAGR',1,lgncmp)
        if (icmp .eq. 0) then
            call utmess('F', 'ASSEMBLA_9')
        endif
        if (icmp .gt. 30) then
            call utmess('F', 'ASSEMBLA_10')
        endif
!
        icodla = lshift(1,icmp)
!
        do 120 ima = 1, nbsma
            nomacr = zk8(ianmcr-1+ima)
            call dismoi('NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', repk=num2)
            call jeveuo(nomacr//'.CONX', 'L', iaconx)
            call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', iaprno)
            if (zi(iasssa-1+ima) .eq. 1) then
                call jeveuo(jexnum(noma//'.SUPMAIL', ima), 'L', iamail)
                call jelira(jexnum(noma//'.SUPMAIL', ima), 'LONMAX', nbnm)
!
                do 110 i = 1, nbnm
                    ino = zi(iamail-1+i)
                    inold = zi(iaconx-1+3* (i-1)+2)
                    if (ino .gt. nm) then
!
! ---        CAS D'UN NOEUD DE LAGRANGE :
!            --------------------------
                        zi(iaprnm-1+nec* (ino-1)+1) = ior(zi(iaprnm-1+ nec* (ino- 1)+1), icodla)
                    else if (inold.gt.0) then
!
! ---        CAS D'UN NOEUD PHYSIQUE DU MAILLAGE :
!            -----------------------------------
                        do 100 iec = 1, nec
                            zi(iaprnm-1+nec* (ino-1)+iec) = ior(&
                                                            zi(iaprnm-1+ nec* (ino-1)+iec),&
                                                            zi(&
                                                            iaprno-1+ ( nec+2)* (inold-1)+2+ iec)&
                                                            )
100                     continue
                    else
                        call utmess('F', 'CALCULEL2_24')
                    endif
110             continue
            endif
120     continue
    endif
    call jedetr('&MAILLA            .NOMA')
!
    call jedema()
!
end subroutine
