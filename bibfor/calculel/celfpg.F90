subroutine celfpg(celz, nomobj, iret)
    implicit  none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/wkvect.h"
    character(len=*) :: celz, nomobj
    integer :: iret
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
! ------------------------------------------------------------------
! BUT : EXTRAIRE DU CHAM_ELEM (ELGA) CELZ UN OBJET JEVEUX CONTENANT
!       LE SCHEMA DE POINT DE GAUSS DES MAILLES
! ------------------------------------------------------------------
!     ARGUMENTS:
! CELZ    IN/JXIN  K19 : SD CHAM_ELEM A EXAMINER
!         REMARQUE:   SI CELZ N'EST PAS "ELGA", NOMOBJ N'EST PAS CREE
!
! NOMOBJ  IN/JXVAR K24 : OBJET JEVEUX A CREER (SUR LA BASE 'V')
!    EN SORTIE, L'OBJET NOMOBJ EST UN VECTEUR DE K16 DIMENSIONNE AU
!    NOMBRE DE MAILLES DU MAILLAGE.
!    V(IMA)(1: 8) : NOM DE L'ELREFE POUR LA MAILLE IMA (OU ' ')
!    V(IMA)(9:16) : NOM DE LA FAMILLE DE PG POUR LA MAILLE IMA (OU ' ')
!
!    SI LA FAMILLE DE PG EST UNE FAMILLE "LISTE" (PAR EXEMPLE MATER),
!    V(IMA) CONTIENT LE NUMERO (<0) DE LA FAMILLE. EX : "-185"
!
!    REMARQUE :
!      SI L'OBJET NOMOBJ EXISTE AVANT L'APPEL A CELFPG :
!      * ON VERFIE QUE LE LIGREL EST LE MEME QUE CELUI QUI A SERVI A
!      CREER NOMOBJ
!      * ON NE LE RECALCULE PAS MAIS ON VA VERIFIER
!      QUE LA FAMILLE DES ELEMENTS DE CELZ EST COHERENTE AVEC CELLE
!      DECRITE DANS NOMOBJ. (CAS D'UNE BOUCLE SUR LES NOMSYM)
!      * ON PEUT NE FAIRE LA VERIF QUE SUR LE 1ER ELEMENT DE CHAQUE GREL
!      PUISQUE LE LIGREL EST LE MEME.
!
!
! IRET    CODE RETOUR
!         0 : PAS D'ERREUR
!         1 : CELZ NE CORRESPOND PAS A NOMOBJ (S'IL EXISTE)
    logical :: lexi
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: ma, nomgd, kbid
    character(len=16) :: nofpg
    character(len=19) :: cel, ligrel, ligrsv
    character(len=24) :: nomosv
    integer :: jobj, ibid, nbma, jcelv, jceld, nec
    integer :: igr, iel, ialiel, illiel
    integer :: jcelk, nbgr, imolo, jmolo, numa, nbel, kfpg
    integer :: iexi
    save ligrsv,nomosv
!
#define numail(igr,iel) zi(ialiel-1+zi(illiel+igr-1)+iel-1)
!     ------------------------------------------------------------------
!
    call jemarq()
    cel=celz
    iret=0
!
!     -- SI CE N'EST PAS UN CHAMP ELGA, IL N'Y A RIEN A FAIRE :
    call jeveuo(cel//'.CELK', 'L', jcelk)
    if (zk24(jcelk-1+3) .ne. 'ELGA') goto 30
!
!
!     1 CALCUL DE LIGREL,NBMA,NEC :
!     --------------------------------------------------------
    call dismoi('F', 'NOM_MAILLA', cel, 'CHAM_ELEM', ibid,&
                ma, ibid)
    call dismoi('F', 'NOM_LIGREL', cel, 'CHAM_ELEM', ibid,&
                ligrel, ibid)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                kbid, ibid)
    call dismoi('F', 'NOM_GD', cel, 'CHAM_ELEM', ibid,&
                nomgd, ibid)
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ibid)
!
!
!     2 RECUPERATION DES OBJETS DU CHAM_ELEM ET DU LIGREL :
!     -------------------------------------------------------
    call jeveuo(cel//'.CELV', 'L', jcelv)
    call jeveuo(cel//'.CELD', 'L', jceld)
    call jeveuo(ligrel//'.LIEL', 'L', ialiel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', illiel)
    nbgr=zi(jceld-1+2)
!
!
!     3 REPLISSAGE DE NOMOBJ :
!     -------------------------------------------------------
!
    call jeexin(nomobj, iexi)
    lexi=(iexi.gt.0)
!
    if (.not.lexi) then
        call wkvect(nomobj, 'V V K16', nbma, jobj)
        ligrsv=ligrel
        nomosv=nomobj
    else
        ASSERT(nomobj.eq.nomosv)
        ASSERT(ligrel.eq.ligrsv)
        call jeveuo(nomobj, 'L', jobj)
    endif
!
    do 20,igr=1,nbgr
    nbel=nbelem(ligrel,igr)
    imolo=zi(jceld-1+zi(jceld-1+4+igr)+2)
    if (imolo .eq. 0) goto 20
    call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
    kfpg=zi(jmolo-1+4+nec+1)
    if (kfpg .gt. 0) then
        call jenuno(jexnum('&CATA.TM.NOFPG', kfpg), nofpg)
    else
        call codent(kfpg, 'G', nofpg)
    endif
!
    if (.not.lexi) then
        do 10,iel=1,nbel
        numa=numail(igr,iel)
        if (numa .gt. 0) zk16(jobj-1+numa)=nofpg
10      continue
!
    else
        iel=1
        numa=numail(igr,iel)
        if (zk16(jobj-1+numa) .ne. nofpg) then
            iret=1
            goto 30
        endif
    endif
    20 end do
!
!
30  continue
    call jedema()
end subroutine
