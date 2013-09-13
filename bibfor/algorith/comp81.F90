subroutine comp81(nomres, basmod, raidf, noma)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/rsadpa.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomres, noma, basmod
    character(len=19) :: raidf
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       COMPATIBILITE MACR_ELEM_DYNA/MACR_ELEM_STAT
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMRES    : NOM UTILISATEUR DU RESULTAT
! IN   BASMOD    : NOM UT DE LA BASE MODALE DE PROJECTION
! IN   RAIDF     : NOM UT DE LA MATRICE RAIDEUR A PROJETER
! IN   MASSEF    : NOM UT DE LA MATRICE DE MASSE A PROJETER
! IN   AMORF     : NOM UT DE LA MATRICE D'AMORTISSEMENT A PROJETER
! IN   MAILLA    : NOM UT DU MAILLAGE EN AMONTC
!
!      SORTIE :
!-------------
!
! ......................................................................
!
!
!
!
    integer :: iarefm, iret, ibid, nbnoe, lldef, iaconx
    integer :: nbmtot, nbmdef, ier
    integer :: nbmdyn, nbndyn, i, j, k, inebid, nec, ie, ierd
    integer :: iacon1, iadesc, iadesm, ialica, ialich, iaprno, icas
    integer :: igex, instdy, iocc, ival, ldgn, ldgn0, lnocmp
    integer :: n1, nbndef, nbno, nbno2, nbnot, ncmpmx, nocc, nueq, nunot
!
    real(kind=8) :: rbndyn, rbndef
!
    character(len=8) :: nomo, blanc, lintf, k8bid, chmat, chcar, nogdsi
    character(len=8) :: nomcas, vectas
    character(len=24) :: gnex
    character(len=14) :: numddl
    character(len=19) :: nu
!
    logical :: lredu
!
    data blanc         /'        '/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
    nu = nomres
    nu = nu(1:14)//'.NUME'
    lredu = .false.
!
! **********************
!     RECUPERATION DES INFOS UTILES
! **********************
    call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid,&
                numddl, ier)
    call dismoi('C', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid,&
                lintf, ier)
!
    call dismoi('F', 'NOM_MODELE', numddl, 'NUME_DDL', ibid,&
                nomo, iret)
    if (raidf .ne. blanc) then
        call dismoi('F', 'CHAM_MATER', raidf, 'MATR_ASSE', ibid,&
                    chmat, iret)
        call dismoi('F', 'CARA_ELEM', raidf, 'MATR_ASSE', ibid,&
                    chcar, iret)
    else
        chmat = blanc
        chcar = blanc
    endif
!
    if (lintf .ne. blanc) then
! ON RECUPERE LE NBRE DE NOEUDS PRESENTS DANS INTERF_DYNA
        call jelira(jexnum(lintf//'.IDC_LINO', 1), 'LONMAX', nbnoe)
! ON RECUPERE LE LISTE DES NOEUDS PRESENTS DANS INTERF_DYNA
        call jeveuo(lintf//'.IDC_DEFO', 'L', lldef)
    else
        nbnoe=0
    endif
    call jeveuo(nomres//'.MAEL_MASS_DESC', 'L', iadesc)
    call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbmtot,&
                k8bid, ier)
    if (nbmtot .eq. 0) ASSERT(.false.)
    call dismoi('F', 'NB_MODES_STA', basmod, 'RESULTAT', nbmdef,&
                k8bid, ier)
    nbmdyn=nbmtot-nbmdef
    if (nbmdyn .lt. 0) ASSERT(.false.)
!
    if (nbmtot .ne. zi(iadesc+1)) then
        call utmess('I', 'ALGORITH_52')
    endif
!
! **********************
!     CREATION DU .NUME
! **********************
    call copisd('NUME_DDL', 'G', numddl, nu)
!
    call dismoi('F', 'NOM_GD', nu(1:14), 'NUME_DDL', ibid,&
                nogdsi, ierd)
    call dismoi('F', 'NB_EC', nogdsi, 'GRANDEUR', nec,&
                k8bid, ierd)
!
! IL FAUT CHOISIR NBNDYN QUI NE SOIENT PAS SUR L'INTERFACE ET POSSEDANT
! NCMPMX COMPOSANTES.
    call jelira(jexnum(nu(1:19)//'.PRNO', 1), 'LONMAX', n1)
    call jeveuo(jexnum(nu(1:19)//'.PRNO', 1), 'L', iaprno)
    nbno = n1/(nec+2)
    k=1
    ncmpmx = 0
    do 553 i = 1, nbno
        nunot=zi(iaprno-1+ (i-1)* (nec+2)+1)
        if (nunot .ne. 0) then
            nueq = zi(iaprno-1+ (i-1)* (nec+2)+2)
            ncmpmx = max(ncmpmx,nueq)
        endif
553  continue
! ON VA CHOISIR PLUSIEURS NOEUDS QUI NE SONT PAS PRESENTS DANS
! L'INTERFACE ET TELS QUE LE NBRE DE DDL CONSIDERE SOIT EGAL
! AU NBRE DE MODES DYNAMIQUES
! ON PREND COMME POSTULAT QUE NBNDYN=PARTIE_ENTIERE DE NBMDYN/NCMPMX
    call getvtx(' ', 'SANS_GROUP_NO', scal=gnex, nbret=igex)
    if (igex .ne. 0) then
        call jelira(jexnom(noma//'.GROUPENO', gnex), 'LONUTI', nbno2)
        call jeveuo(jexnom(noma//'.GROUPENO', gnex), 'L', ldgn0)
        call wkvect('&&COMP81.NEUEXC', 'V V I', nbno2, ldgn)
        do 557 j = 1, nbno2
            zi(ldgn+j-1)=zi(ldgn0+j-1)
557      continue
    else
        nbno2=nbnoe
        if (nbno2 .ne. 0) then
            call wkvect('&&COMP81.NEUEXC', 'V V I', nbno2, ldgn)
            do 558 j = 1, nbno2
                zi(ldgn+j-1)=zi(lldef+j-1)
558          continue
        else
            call wkvect('&&COMP81.NEUEXC', 'V V I', 1, ldgn)
            zi(ldgn) = 0
        endif
    endif
    nbndyn=nbmdyn/ncmpmx
    rbndyn=dble(nbmdyn)/dble(ncmpmx)
    if (abs(rbndyn-dble(nbndyn)) .gt. 0.d0) then
        call utmess('I', 'ALGORITH_53', si=ncmpmx)
    endif
    if (nbndyn .eq. 0) then
        call wkvect(nomres//'.NEUBID', 'V V I', 1, inebid)
        zi(inebid) = 0
        goto 554
    endif
    call wkvect(nomres//'.NEUBID', 'V V I', nbndyn, inebid)
    do 555 i = 1, nbno
        nunot=zi(iaprno-1+ (i-1)* (nec+2)+1)
        if (nunot .ne. 0) then
            nueq = zi(iaprno-1+ (i-1)* (nec+2)+2)
            if (nueq .eq. ncmpmx) then
                do 556 j = 1, nbno2
                    if (i .eq. zi(ldgn+j-1)) goto 555
556              continue
                zi(inebid+k-1)= i
                if (k .eq. nbndyn) goto 554
                k=k+1
            endif
        endif
555  continue
!
554  continue
    if (nbmdef .ne. 0) then
        call rsadpa(basmod, 'L', 1, 'NOEUD_CMP', nbmdyn+1,&
                    0, lnocmp, k8bid)
        if (zk16(lnocmp) .eq. ' ') lredu=.true.
    endif
    if (lredu) then
        nbndef=nbmdef/ncmpmx
        rbndef=dble(nbmdef)/dble(ncmpmx)
        if (abs(rbndef-dble(nbndef)) .gt. 0.d0) then
            call utmess('I', 'ALGORITH_54', si=ncmpmx)
        endif
        if (nbndyn .ne. 0) then
            nbnot = nbno2 + nbndyn
            call juveca('&&COMP81.NEUEXC', nbnot)
            call jeveuo('&&COMP81.NEUEXC', 'E', ldgn)
            do 651 j = nbno2+1, nbnot
                zi(ldgn+j-1)=zi(inebid+j-1-nbno2)
651          continue
            nbno2 = nbnot
        endif
        call wkvect('&&COMP81.NOSTDY', 'V V I', nbndef, instdy)
        k=1
        do 655 i = 1, nbno
            nunot=zi(iaprno-1+ (i-1)* (nec+2)+1)
            if (nunot .ne. 0) then
                nueq = zi(iaprno-1+ (i-1)* (nec+2)+2)
                if (nueq .eq. ncmpmx) then
                    do 656 j = 1, nbno2
                        if (i .eq. zi(ldgn+j-1)) goto 655
656                  continue
                    zi(instdy+k-1)= i
                    if (k .eq. nbndef) goto 654
                    k=k+1
                endif
            endif
655      continue
!
654      continue
    else
        if (nbnoe .ne. 0) then
            call wkvect('&&COMP81.NOSTDY', 'V V I', nbnoe, instdy)
            do 658 j = 1, nbnoe
                zi(instdy+j-1)=zi(lldef+j-1)
658          continue
        else
            call wkvect('&&COMP81.NOSTDY', 'V V I', 1, instdy)
            zi(instdy) = 0
        endif
        nbndef = nbnoe
    endif
!
! **********************
!     CREATION DU .REFM
! **********************
    call wkvect(nomres//'.REFM', 'G V K8', 8, iarefm)
! STOCKAGE DU NOM DU MODELE
    zk8(iarefm-1+1)= nomo
! STOCKAGE DU NOM DU MAILLAGE
    zk8(iarefm-1+2)= noma
! STOCKAGE DU NOM DU CHAMP DE MATERIAU
    zk8(iarefm-1+3)=chmat
! STOCKAGE DU NOM DU CHAMP DE CARACTERISTIQUES ELEMENTAIRES
    zk8(iarefm-1+4)=chcar
! STOCKAGE DU NOM DE LA NUMEROTATION
    zk8(iarefm-1+5)=nu
! STOCKAGE DU NOM DU CHAMP DE CARACTERISTIQUES ELEMENTAIRES
    zk8(iarefm-1+6)= 'OUI_RIGI'
    zk8(iarefm-1+7)= 'OUI_MASS'
    zk8(iarefm-1+8)= 'NON_AMOR'
!
! **********************
!     CREATION DU .DESM
! **********************
    call wkvect(nomres//'.DESM', 'G V I', 10, iadesm)
!
! METTRE ICI LE NBRE DE ?
    zi(iadesm-1+1)= 0
! METTRE ICI LE NBRE DE NOEUD EXTERIEUR NON DUPLIQUES
    zi(iadesm-1+2)=nbndef+nbndyn
! METTRE ICI LE NBRE DE NOEUDS INTERNES
    zi(iadesm-1+3)=nbno
! METTRE ICI LE NBRE DE DDL EXTERIEUR
    zi(iadesm-1+4)=zi(iadesc+1)
! METTRE ICI LE NBRE DE DDL INTERIEUR (OU TOTAL)
    zi(iadesm-1+5)=0
! METTRE ICI LE NBRE DE CHARGEMENT
    zi(iadesm-1+6)=0
    zi(iadesm-1+7)=0
! METTRE ICI LE NBRE DE LAGRANGE EXTERNE
    zi(iadesm-1+8)=0
! METTRE ICI LE NBRE DE LAGRANGE LIAISON
    zi(iadesm-1+9)=0
! METTRE ICI LE NBRE DE LAGRANGE INTERNE
    zi(iadesm-1+10)=0
!
    if ((nbndef+nbndyn) .eq. 0) goto 669
!
! **********************
!     CREATION DU .LINO
! **********************
    call wkvect(nomres//'.LINO', 'G V I', nbndef+nbndyn, iaconx)
    do 665 i = 1, nbndyn
        zi(iaconx+i-1)=zi(inebid+i-1)
665  continue
    do 666 i = nbndyn+1, nbndef+nbndyn
        zi(iaconx+i-1)=zi(instdy+i-nbndyn-1)
666  continue
!
! **********************
!     CREATION DU .CONX
! **********************
    call wkvect(nomres//'.CONX', 'G V I', 3*(nbndef+nbndyn), iacon1)
    do 667 i = 1, nbndyn
        zi(iacon1+3*i-3)=1
        zi(iacon1+3*i-2)=zi(inebid+i-1)
        zi(iacon1+3*i-1)=0
667  continue
    do 668 i = nbndyn+1, nbndef+nbndyn
        zi(iacon1+3*i-3)=1
        zi(iacon1+3*i-2)=zi(instdy+i-nbndyn-1)
        zi(iacon1+3*i-1)=0
668  continue
669  continue
!
! **********************
!     CREATION DU .KP_EE
! **********************
    call jeexin(nomres//'.MAEL_AMOR_VALE', iret)
    if (iret .gt. 0) then
        zk8(iarefm-1+8)= 'OUI_AMOR'
    endif
!
!     -- CREATION DES OBJETS .LICA ET .LICH:
!     --------------------------------------
    call getfac('CAS_CHARGE', nocc)
    if (nocc .ne. 0) then
        call jecrec(nomres//'.LICA', 'G V R', 'NO', 'DISPERSE', 'CONSTANT',&
                    nocc)
        call jecrec(nomres//'.LICH', 'G V K8', 'NO', 'CONTIG', 'CONSTANT',&
                    nocc)
        call jeecra(nomres//'.LICA', 'LONMAX', 2*nbmtot)
        call jeecra(nomres//'.LICH', 'LONMAX', 2)
!
        do 670, iocc= 1,nocc
        call getvtx('CAS_CHARGE', 'NOM_CAS', iocc=iocc, scal=nomcas, nbret=n1)
        call getvid('CAS_CHARGE', 'VECT_ASSE_GENE', iocc=iocc, scal=vectas, nbret=n1)
        call jecroc(jexnom(nomres//'.LICA', nomcas))
        call jecroc(jexnom(nomres//'.LICH', nomcas))
        call jenonu(jexnom(nomres//'.LICA', nomcas), icas)
        call jeveuo(jexnum(nomres//'.LICA', icas), 'E', ialica)
        call jeveuo(jexnum(nomres//'.LICH', icas), 'E', ialich)
        call jeveuo(vectas//'           .VALE', 'L', ival)
        do 671, ie= 1,nbmtot
        zr(ialica+ie-1) = zr(ival+ie-1)
        zr(ialica+nbmtot+ie-1) = zr(ival+ie-1)
671      continue
        zk8(ialich)='NON_SUIV'
        zk8(ialich+1)=vectas
        zi(iadesm-1+7)=icas
670      continue
    endif
!
! --- MENAGE
!
    call jedetr('&&COMP81.NEUEXC')
    call jedetr('&&COMP81.NOSTDY')
!
    call jedema()
end subroutine
