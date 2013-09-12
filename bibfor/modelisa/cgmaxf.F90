subroutine cgmaxf(mofaz, iocc, nomaz, lismaz, nbma)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/gmgnre.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/xtmafi.h"
!
    integer :: iocc, nbma
    character(len=*) :: mofaz, nomaz, lismaz
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
!       CGMAXF -- TRAITEMENT DE L'OPTION FISS_XFEM
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES
!                                   DU TYPE XFEM DEMANDE PAR
!                                   L'UTILISATEUR
!  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!
    integer :: i, ii, ima, ifiss, ino, n
    integer :: nbno, nbnot, nfiss, nmax, nbmalo, nbmala
    integer :: jlmas, idlist, jfiss, jtem3, jtem4, jtem5, jstno
    integer :: ibid, iret, test, valeno
    character(len=8) :: noma, k8bid, nomail, fiss
    character(len=16) :: motfac, typgrp
    character(len=19) :: stno
    character(len=24) :: lismai, lismar, lisman, maifis
    integer :: iarg
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ================
    nbmala=0
    nbma = 0
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
!
! --  RECUPERATION DU TYPE DE MAILLE XFEM :
    call getvtx(motfac, 'TYPE_GROUP', iocc=iocc, scal=typgrp, nbret=ibid)
    call getvid(motfac, 'FISSURE', iocc=iocc, nbval=0, nbret=nfiss)
    nfiss = -nfiss
    call wkvect('&&CGMAXF.FISS', 'V V K8', nfiss, jfiss)
    call getvid(motfac, 'FISSURE', iocc=iocc, nbval=nfiss, vect=zk8(jfiss),&
                nbret=ibid)
!
!
! --- TYPE DE MAILLE = 'HEAVISIDE', 'CRACKTIP' OU  'MIXTE'
!     ====================================================
    if ((typgrp.eq.'HEAVISIDE') .or. (typgrp.eq.'CRACKTIP') .or. (typgrp.eq.'MIXTE')) then
!
        call wkvect('&&CGMAXF.TEM3', 'V V I', nfiss, jtem3)
        call wkvect('&&CGMAXF.TEM4', 'V V I', nfiss, jtem4)
!
! ---   TYPE DE MAILLE = 'HEAVISIDE'
        if (typgrp .eq. 'HEAVISIDE') then
            maifis = '.MAILFISS.HEAV'
!
! ---   TYPE DE MAILLE = 'CRACKTIP'
        else if (typgrp.eq.'CRACKTIP') then
            maifis = '.MAILFISS.CTIP'
!
! ---   TYPE DE MAILLE =
        else if (typgrp.eq.'MIXTE') then
            maifis = '.MAILFISS.HECT'
        endif
! ---   BOUCLE SUR TOUTES LES FISSURES
        do 10 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            call jeveuo(fiss//maifis, 'L', zi(jtem3-1+ifiss))
            call jelira(fiss//maifis, 'LONMAX', n)
            zi(jtem4-1+ifiss) = n
            nbma = nbma + n
10      continue
! ---   CREATION ET REMPLISSAGE DU VECTEUR DE SORTIE
        if (nbma .gt. 0) then
            call wkvect(lismai, 'V V I', nbma, idlist)
            do 40 ifiss = 1, nfiss
                jlmas = zi(jtem3-1+ifiss)
                do 20 i = 1, zi(jtem4-1+ifiss)
                    nbmala = nbmala + 1
                    zi(idlist+nbmala-1) = zi(jlmas+i-1)
                    call jenuno(jexnum(noma//'.NOMMAI', zi(jlmas+i-1)), nomail)
20              continue
40          continue
        endif
        call jedetr('&&CGMAXF.TEM3')
        call jedetr('&&CGMAXF.TEM4')
!
! --- TYPE DE MAILLE = 'XFEM'
!     ============================
    else if (typgrp.eq.'XFEM') then
!
        lisman = '&&CGMAXF.TEM1'
        call xtmafi(noma, 0, zk8(jfiss), nfiss, lismai,&
                    lisman, nbma)
        call jedetr(lisman)
!
! --- TYPE DE MAILLE = 'FISSUREE'
!     ============================
    else if (typgrp.eq.'FISSUREE') then
!
        call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnot,&
                    k8bid, iret)
        call dismoi('F', 'NB_NO_MAX', '&CATA', 'CATALOGUE', nmax,&
                    k8bid, iret)
!
!       POUR DIMENSIONNER GROSSIEREMENT LA LISTE DES MAILLES
        lisman = '&&CGMAXF.TEM1'
        lismar = '&&CGMAXF.TEM2'
        call xtmafi(noma, 0, zk8(jfiss), nfiss, lismar,&
                    lisman, nbma)
        call jedetr(lismar)
        call jedetr(lisman)
!
        call wkvect('&&CGMAXF.TEM3', 'V V I', nbnot, jtem3)
        call wkvect('&&CGMAXF.TEM4', 'V V I', nmax, jtem4)
        call wkvect('&&CGMAXF.TEM5', 'V V I', nbma, jtem5)
!
        nbmalo = 0
!
!       POUR CHAQUE FISSURE
        do 50 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            stno = fiss//'.STNO'
            call jeveuo(stno//'.VALE', 'L', jstno)
!
!         RECUPERATION DE TOUTES MAILLES XFEM DE LA FISSURE COURANTE
            lisman = '&&CGMAXF.TEM1'
            call xtmafi(noma, 0, fiss, 1, lismar,&
                        lisman, nbmala)
            call jeveuo(lismar, 'L', jlmas)
!
!         POUR CHAQUE MAILLE XFEM DE LA FISSURE COURANTE
            do 51 ii = 1, nbmala
!           RECUPERATION DES NOEUDS
                call gmgnre(noma, nbnot, zi(jtem3), zi(jlmas+ii-1), 1,&
                            zi(jtem4), nbno, 'TOUS')
!           TRI DES NOEUDS SELON LEUR STATUS XFEM
                test = 1
                do 511 ino = 1, nbno
                    valeno = zi(jstno+zi(jtem4+ino-1)-1)
                    if (valeno .eq. 0) then
                        test = 0
                    endif
511              continue
!           MAILLES QUI REPOSENT SUR LES NOEUDS AU STATUS <> 0
                if (test .eq. 1) then
                    nbmalo = nbmalo + 1
                    zi(jtem5+nbmalo-1) = zi(jlmas+ii-1)
                    call jenuno(jexnum(noma//'.NOMMAI', zi(jtem5+ nbmalo-1)), nomail)
                endif
51          continue
            call jedetr(lismar)
            call jedetr(lisman)
            call jedetr(stno)
50      continue
        call wkvect(lismai, 'V V I', nbmalo, idlist)
        do 60 ima = 1, nbmalo
            zi(idlist-1+ima) = zi(jtem5-1+ima)
60      continue
        call jedetr('&&CGMAXF.TEM3')
        call jedetr('&&CGMAXF.TEM4')
        call jedetr('&&CGMAXF.TEM5')
        nbma = nbmalo
!
    endif
!
! --- FIN
!     ===
!
! --- MENAGE
!
    call jedetr('&&CGMAXF.FISS')
!
    call jedema()
!
end subroutine
