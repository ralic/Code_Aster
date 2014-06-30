subroutine irchor(ioccur, leresu, lresul, nchsym, nnuord,&
                  nlicmp, novcmp, nnopar, nbnosy, nbordr,&
                  nbrcmp, nbcmdu, nbpara, codret)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/irparb.h"
#include "asterfort/irvcmp.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ioccur, nbnosy, nbordr, nbrcmp, nbcmdu, nbpara, codret
    character(len=8) :: leresu
    character(len=*) :: nchsym, nnuord, nlicmp, novcmp, nnopar
    logical(kind=1) :: lresul
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - CHAMP, NUMEROS D'ORDRE, ...
!  -    -      --               --
! ----------------------------------------------------------------------
!
!  CETTE ROUTINE SCRUTE LES MOTS-CLES TOUT_CHAM, NOM_CMP, ...
!
! IN  :
!   IOCCUR  I    NUMERO D'OCCURENCE DU MOT CLE FACTEUR
!   LERESU  K8   CHAINE CONTENANT SOIT LE NOM DE LA SD RESULTAT SOIT
!                 LE NOM DU CHAMP A IMPRIMER
!   LRESUL  L    BOOLEEN INDIQUANT SI L'UTILISATEUR IMPRIME UN CHAMP
!                 (FALSE) OU UN RESU (TRUE)
!
! IN/OUT :
!   NCHSYM  K*   NOM DE L'OBJET JEVEUX CONTENANT LE NOM DES CHAMPS
!                 A IMPRIMER
!   NNUORD  K*   NOM DE L'OBJET JEVEUX CONTENANT LES NUMEROS D'ORDRE
!   NLICMP  K*   NOM DE L'OBJET JEVEUX CONTENANT LES COMPOSANTES
!   NOVCMP  K*   NOM DE L'OBJET JEVEUX CONTENANT LES NOMS MED
!   NNOPAR  K*   NOM DE L'OBJET JEVEUX CONTENANT LES PARAMETRES
!
! OUT :
!   NBNOSY  I    NOMBRE DE CHAMPS A IMPRIMER
!   NBORDR  I    NOMBRE DE NUMEROS D'ORDRE A IMPRIMER
!   NBRCMP  I    NOMBRE DE COMPOSANTES
!   NBCMDU  I    NOMBRE DE NOMS MED
!   NPARAM  I    NOMBRE DE PARAMETRES (FORMAT 'RESULTAT')
!   CODRET  I    CODE RETOUR (0 SI OK, 1 SINON)
!
!
!
    integer :: jnosy, jncmed, jpa, jordr, n23, iret, n21, nvcmp
    integer :: n22, nnrmed, isy, nnocha, nnocmp, nchar,  ibid, npreci
    integer :: vali, ncrit, innosy, jnordr, icmp, nbcmpt, gd, ncmpmx, iad
    integer :: ntpara, nnpara, jcmp, nparam
!
    real(kind=8) :: prec
!
    character(len=1) :: k1bid
    character(len=3) :: toupar, toucha
    character(len=8) :: resmed, crit, nomgd
    character(len=19) :: noch19, knum
    character(len=24) :: valk(6)
!
    logical(kind=1) :: afaire
    character(len=8), pointer :: veri_nom_cmp(:) => null()
!
    call jemarq()
!
    afaire = .false.
    codret = 0
!
!     --- ECRITURE D'UN CHAM_GD ---
    if (.not.lresul) then
        nbnosy = 1
        call wkvect(nchsym, 'V V K16', nbnosy, jnosy)
        call wkvect(novcmp, 'V V K80', nbnosy, jncmed)
!       --- NOM DU CHAM_GD ---
        zk16(jnosy) = leresu
        nparam = 0
        jpa = 1
        nbordr = 1
        call wkvect(nnuord, 'V V I', nbordr, jordr)
        zi(jordr) = 1
        call getvtx('RESU', 'NOM_CHAM_MED', iocc=ioccur, nbval=0, nbret=n23)
        nbcmdu = - n23
        call getvtx('RESU', 'NOM_CHAM_MED', iocc=ioccur, nbval=nbcmdu, vect=zk80( jncmed),&
                    nbret=iret)
!
!     --- ECRITURE D'UN RESULTAT_COMPOSE ---
    else
!       --- ON REGARDE QUELS SONT LES NOM_CHAM A IMPRIMER:
        toucha = 'OUI'
        call getvtx('RESU', 'TOUT_CHAM', iocc=ioccur, scal=toucha, nbret=n21)
        call getvtx('RESU', 'NOM_CHAM', iocc=ioccur, nbval=0, nbret=n22)
        call getvtx('RESU', 'NOM_CHAM_MED', iocc=ioccur, nbval=0, nbret=n23)
        call getvtx('RESU', 'NOM_RESU_MED', iocc=ioccur, nbval=0, nbret=nnrmed)
!       *** N22 EST NEGATIF SI L'UTILISATEUR DONNE UNE LISTE DE NOMS
!           (PAR DEFAUT TOUS LES CHAMPS CAR MOT-CLE FACULTATIF)
        if (abs(n21)+abs(n22) .eq. 0) n21=1
        if (n21 .gt. 0 .and. toucha .eq. 'OUI' .and. nnrmed .eq. 0) then
!         - ON RECUPERE LES NOMS (ON IMPRIME TOUS LES CHAMPS)
            call jelira(leresu//'           .DESC', 'NOMUTI', nbnosy)
            call wkvect(nchsym, 'V V K16', nbnosy, jnosy)
            do isy = 1, nbnosy
                call jenuno(jexnum(leresu//'           .DESC', isy), zk16(jnosy-1+isy))
            end do
        else if (n21.gt.0 .and. toucha.eq.'NON') then
            nbnosy=0
            jnosy =1
        else if (n22.lt.0) then
            nbnosy = - n22
            nbcmdu = - n23
            call wkvect(nchsym, 'V V K16', nbnosy, jnosy)
            call wkvect(novcmp, 'V V K80', nbnosy, jncmed)
!
!         - ON RECUPERE LA LISTE DES NOMS DONNEE PAR L'UTILISATEUR
            call getvtx('RESU', 'NOM_CHAM', iocc=ioccur, nbval=nbnosy, vect=zk16( jnosy),&
                        nbret=nnocha)
            call getvtx('RESU', 'NOM_CHAM_MED', iocc=ioccur, nbval=nbcmdu, vect=zk80(jncmed),&
                        nbret=iret)
            if ((nbcmdu.ne.0) .and. (nbcmdu.ne.nbnosy)) then
                call utmess('F', 'PREPOST2_1')
            endif
        else if (nnrmed.lt.0) then
            call getvtx('RESU', 'NOM_CMP', iocc=ioccur, nbval=0, nbret=nnocmp)
            if (nnocmp .lt. 0) then
                valk(1)='NOM_CMP'
                valk(2)='NOM_RESU_MED'
                call utmess('F', 'MED2_6', nk=2, valk=valk)
            endif
            call jelira(leresu//'           .DESC', 'NOMUTI', nbcmdu)
            call getvtx('RESU', 'NOM_RESU_MED', iocc=ioccur, scal=resmed, nbret=nnrmed)
            call wkvect(nchsym, 'V V K16', nbcmdu, jnosy)
            call wkvect(novcmp, 'V V K80', nbcmdu, jncmed)
            nbnosy=nbcmdu
            do isy = 1, nbcmdu
                call jenuno(jexnum(leresu//'           .DESC', isy), zk16(jnosy-1+isy))
                zk80(jncmed+isy-1)='________'
                nchar=lxlgut(resmed)
                zk80(jncmed+isy-1)(1:nchar)=resmed(1:nchar)
                nchar=lxlgut(zk16(jnosy-1+isy))
                zk80(jncmed+isy-1)(9:8+nchar)=zk16(jnosy-1+isy)(1:&
                nchar)
            end do
        endif
!
!       --- ON REGARDE QUELS SONT LES NOM_CMP A IMPRIMER:
        call getvtx('RESU', 'NOM_CMP', iocc=ioccur, nbval=0, nbret=nnocmp)
        if (nnocmp .lt. 0) then
            nvcmp=-nnocmp
            AS_ALLOCATE(vk8=veri_nom_cmp, size=nvcmp)
            call getvtx('RESU', 'NOM_CMP', iocc=ioccur, nbval=nvcmp, vect=veri_nom_cmp,&
                        nbret=ibid)
            afaire = .true.
        endif
!
!       *** NOMS DES CHAMPS DANS ZK16 A PARTIR DE ZK16(JNOSY)
!
!       --- NUMEROS D'ORDRE POUR IOCCUR DU MOT-CLE FACTEUR RESU
        knum = nnuord
!       *** TEST DE PRESENCE DES MOTS CLES PRECISION ET CRITERE
        call getvr8('RESU', 'PRECISION', iocc=ioccur, scal=prec, nbret=npreci)
        call getvtx('RESU', 'CRITERE', iocc=ioccur, scal=crit, nbret=ncrit)
!       *** RECUPERATION DES NUMEROS D'ORDRE DE LA STRUCTURE DE
!          DONNEES DE TYPE RESULTAT LERESU A PARTIR DES VARIABLES
!          D'ACCES UTILISATEUR 'NUME_ORDRE','FREQ','INST','NOEUD_CMP'
!           (VARIABLE D'ACCES 'TOUT_ORDRE' PAR DEFAUT)
        call rsutnu(leresu, 'RESU', ioccur, knum, nbordr,&
                    prec, crit, iret)
!       *** SI PB ON PASSE AU FACTEUR SUIVANT DE IMPR_RESU
        if (iret .ne. 0) then
            codret = 1
            goto 999
        endif
        call jeveuo(knum, 'L', jordr)
!
        if (n22 .lt. 0) then
            do innosy = 0, nbnosy-1
                do jnordr = 0, nbordr-1
                    call rsexch(' ', leresu, zk16(jnosy+innosy), zi(jordr+jnordr), noch19,&
                                iret)
                    if (iret .ne. 0) then
                        valk (1) = zk16(jnosy+innosy)
                        vali = zi(jordr+jnordr)
                        call utmess('A', 'POSTRELE_41', sk=valk(1), si=vali)
                    endif
                end do
            end do
            if (afaire) then
                do icmp = 0, nvcmp-1
                    nbcmpt = 0
                    do innosy = 0, nbnosy-1
                        call rsexch(' ', leresu, zk16(jnosy+innosy), zi( jordr), noch19,&
                                    iret)
                        if (iret .eq. 0) then
                            call dismoi('NUM_GD', noch19, 'CHAMP', repi=gd)
                            call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
                            if (nomgd .eq. 'VARI_R') then
!                   TRAITEMENT PARTICULIER POUR LA GRANDEUR VARI_R
                                nbcmpt = nbcmpt + 1
                                goto 17
                            endif
                            call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
                            call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
                            call irvcmp(ncmpmx, zk8(iad), veri_nom_cmp(icmp+1), nbcmpt)
                        endif
 17                     continue
                    end do
                    if (nbcmpt .eq. 0) then
                        valk (1) = veri_nom_cmp(icmp+1)
                        valk (2) = k1bid
                        call utmess('A', 'PREPOST5_61', nk=2, valk=valk)
                    endif
                end do
            endif
        endif
!
!       --- ON RECHERCHE LES PARAMETRES A ECRIRE ---
!           (UNIQUEMENT SI FORMAT FICHIER = 'RESULTAT')
        toupar = 'NON'
        call getvtx('RESU', 'TOUT_PARA', iocc=ioccur, scal=toupar, nbret=ntpara)
        call getvtx('RESU', 'NOM_PARA', iocc=ioccur, nbval=0, nbret=nnpara)
        if (nnpara .eq. 0) ntpara = 1
        if (ntpara .ne. 0 .and. toupar .eq. 'NON') then
            nparam = 0
            jpa = 1
        else if (ntpara.ne.0.and.toupar.eq.'OUI') then
            nparam = -1
            jpa = 1
        else if (nnpara.ne.0) then
            nparam = -nnpara
            call wkvect('&&IRCHOR.NOMUTI_PARA', 'V V K16', nparam, jpa)
            call getvtx('RESU', 'NOM_PARA', iocc=ioccur, nbval=nparam, vect=zk16(jpa))
        endif
    endif
!
!     --- CHOIX DES COMPOSANTES AUX FORMATS ---
!         RESULTAT, CASTEM, MED ET GMSH
    call getvtx('RESU', 'NOM_CMP', iocc=ioccur, nbval=0, nbret=nnocmp)
    if (nnocmp .lt. 0) then
        nbrcmp=-nnocmp
        call wkvect(nlicmp, 'V V K8', nbrcmp, jcmp)
        call getvtx('RESU', 'NOM_CMP', iocc=ioccur, nbval=nbrcmp, vect=zk8(jcmp),&
                    nbret=ibid)
    endif
!
!     - VERIFICATION DES PARAMETRES (FORMAT 'RESULTAT')
    call irparb(leresu, nparam, zk16(jpa), nnopar, nbpara)
!
999 continue
    AS_DEALLOCATE(vk8=veri_nom_cmp)
    call jedetr('&&IRCHOR.NOMUTI_PARA')
!
    call jedema()
!
end subroutine
