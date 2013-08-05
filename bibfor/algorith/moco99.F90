subroutine moco99(nomres, resul, nbmod, lrang, iorne,&
                  seul)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesg.h"
    integer :: nbmod, lrang(nbmod), iorne
    character(len=8) :: nomres, resul
    logical :: seul
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
!     BUT:
!       POINTER LES PREMIERS MODES PROPRES D'UNE STRUCTURE RESULTAT
!       DE TYPE MODE_MECA DANS UNE AUTRE STRUCTURE DE TYPE BASE_MODALE
!       DEJA EXISTANTE A PARTIR D'UN NUMERO D'ORDRE
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMRES    : NOM UTILISATEUR DE LA STRUCTURE RESULTAT A REMPLIR
! IN   RESUL     : NOM DE LA STRUCTURE RESULTAT A POINTER
! IN   NBMOD     : NOMBRE DE MODES A POINTER
! IN   LRANG     : LISTE DES ANCIENS NUMEROS DE RANGEMENT A POINTER
! IN   SEUL      : INDICATEUR POUR UNE DEUXIEME OCCURENCE.
!                  MODE_MECA OU MODE_STAT ETC. SOUS LE MOT CLE "RITZ"
!
!      SORTIE :
!-------------
! OUT  IORNE     : NUMERO D'ORDRE DU PREMIER CHAMPS 'DEPL' A POINTER
!
! ......................................................................
!
!
!
!
    integer :: nbpabm
    parameter   (nbpabm=10)
    integer :: ldpar(nbpabm), ldpa2(nbpabm)
    integer :: nbcham, nbold, nbtrou, vali
    integer :: i, ii, jrba, jref, jtyp, ier, iorol, ire, ibid, jordr
    integer :: llkge, llmge, llncp, llom2, lltmo, llval2, llvalo
!
    real(kind=8) :: genek, genem, omeg2, rbid, epsi
!
    character(len=1) :: k1bid
    character(len=8) :: k8bid, interf, typi
    character(len=16) :: typres, bmpara(nbpabm), chmeca, typmo
    character(len=19) :: chamol, chamne
    character(len=24) :: type, typeba
!
    complex(kind=8) :: cbid
!
!-----------------------------------------------------------------------
!
    data  bmpara /'NUME_MODE','FREQ','NORME','NOEUD_CMP','TYPE_DEFO',&
     &              'OMEGA2','MASS_GENE','RIGI_GENE','TYPE_MODE',&
     &              'AMOR_REDUIT'/
!
!-----------------------------------------------------------------------
!
! --- CAS DE L'ABSENCE D'UN MODE_MECA
!
    if (resul .eq. '          ' .or. nbmod .eq. 0) goto 9999
!
!
! --- DETERMINATION DU NOMBRE DE MODES DANS LA STRUCTURE A POINTER
!
    call rsorac(resul, 'LONUTI', ibid, rbid, k8bid,&
                cbid, epsi, 'ABSOLU', nbold, 1,&
                nbtrou)
!
    if (nbmod .gt. nbold) then
        vali = nbold
        call u2mesg('I', 'ALGORITH13_48', 0, ' ', 1,&
                    vali, 0, 0.d0)
        nbmod=nbold
    endif
    nbmod=min(nbmod,nbold)
!
    if (nbmod .eq. 0) goto 9999
!     --- ON RECUPERE LE TYPE D'INTERFACE ---
!
    call jeveuo(nomres//'           .REFD', 'L', jref)
    interf = zk24(jref+4) (1:8)
    if (interf .ne. ' ') then
        type = interf//'.IDC_TYPE'
        call jeveuo(type, 'L', jtyp)
        typi = zk8(jtyp)
    endif
!
! --- RECHERCHE DE L'ADRESSE DES ANCIENNES VALEURS PROPRES
!
    call dismoi('F', 'TYPE_RESU', resul, 'RESULTAT', ibid,&
                typres, ire)
!
    typeba=' '
    if (typres .ne. 'MULT_ELAS') then
        call jeveuo(resul//'           .REFD', 'L', jrba)
        typeba=zk24(jrba+6)
    endif
!
    typmo=' '
!
!
! ----- RECUPERATION DU NOMBRE DE CHAMP POSSIBLE DE LA SD
    call jelira(resul//'           .DESC', 'NOMMAX', nbcham, k1bid)
!
    call jeveuo(resul//'           .ORDR', 'L', jordr)
    do 10 i = 1, nbmod
!       LRANG : CONTIENT DES NUMEROS DE RANGEMENT : 1,2, ..., NBMOD
        ASSERT(lrang(i).eq.i)
        iorol=zi(jordr-1+lrang(i))
!
! ------BOUCLE SUR LA LISTE DE CHAMPS AFIN D'IDENTIFIER CEUX QUI
!       SONT PRESENTES DANS L'ANCIEN RESULT
!
        do 30 ii = 1, nbcham
! ----- RECUPERATION DU NOM DU CHAMP POSSIBLE DE LA SD
            call jenuno(jexnum(resul//'           .DESC', ii), chmeca)
!
! ----- REQUETE NOM ET ADRESSE ANCIEN CHAMNO
            call rsexch(' ', resul, chmeca, iorol, chamol,&
                        ier)
            if (ier .eq. 0) then
                call rsexch(' ', nomres, chmeca, iorne, chamne,&
                            ier)
                if (chamol .ne. chamne) then
                    call copisd('CHAMP', 'G', chamol, chamne)
                endif
                call rsnoch(nomres, chmeca, iorne)
            endif
30      continue
!
        if (typres .ne. 'MODE_MECA') goto 11
!
! ----- RECUPERATION DES VALEURS GENERALISEES ET PULSATION CARREE
!
        call rsadpa(resul, 'L', 1, 'RIGI_GENE', iorol,&
                    0, llkge, k8bid)
        genek=zr(llkge)
        call rsadpa(resul, 'L', 1, 'MASS_GENE', iorol,&
                    0, llmge, k8bid)
        genem=zr(llmge)
        call rsadpa(resul, 'L', 1, 'OMEGA2', iorol,&
                    0, llom2, k8bid)
        omeg2=zr(llom2)
!
        call rsadpa(resul, 'L', 1, 'TYPE_MODE', iorol,&
                    0, lltmo, k8bid)
        typmo=zk16(lltmo)
!
11      continue
!
! ----- ECRITURE DES NOUVEAUX PARAMETRES
!
        call rsadpa(nomres, 'E', nbpabm, bmpara, iorne,&
                    0, ldpar, k8bid)
        zi(ldpar(1))=iorne
        if (typmo(1:8) .ne. 'MODE_DYN') then
            if (typeba(1:1) .ne. ' ') then
                call rsadpa(resul, 'L', nbpabm, bmpara, iorol,&
                            0, ldpa2, k8bid)
                zr(ldpar(2)) = zr(ldpa2(2))
                zk24(ldpar(3)) = zk24(ldpa2(3))
                zk16(ldpar(4)) = zk16(ldpa2(4))
                zk16(ldpar(5)) = zk16(ldpa2(5))
                zr(ldpar(6)) = zr(ldpa2(6))
                zr(ldpar(7)) = zr(ldpa2(7))
                zr(ldpar(8)) = zr(ldpa2(8))
                if (.not.seul) then
                    zk16(ldpar(9)) ='MODE_STA'
                else if (seul) then
                    zk16(ldpar(9)) = zk16(ldpa2(9))
                endif
                goto 12
            endif
            zr(ldpar(2))=0.d0
            zk24(ldpar(3))=' '
            zk16(ldpar(4))=' '
            zk16(ldpar(5))=' '
            if (typmo(1:8) .eq. 'MODE_STA') then
                call rsadpa(resul, 'L', 1, 'NOEUD_CMP', iorol,&
                            0, llncp, k8bid)
                zk16(ldpar(4)) = zk16(llncp)
                zk16(ldpar(5)) = 'STATIQUE'
                if (interf .ne. ' ') then
                    if (typi .eq. 'CRAIGB') zk16(ldpar(5)) = 'CONTRAINT'
                    if (typi .eq. 'MNEAL') zk16(ldpar(5)) = 'ATTACHE'
                endif
            endif
            zr(ldpar(6))=0.d0
            zr(ldpar(7))=0.d0
            zr(ldpar(8))=0.d0
! ON ETEND LA DECLARATION MODE_STA A TOUS LES MODES NON DYNA PAR
! EXEMPLE LES MULT_ELAS POUR ETRE COMPTABILISES COMME MODES STATIQUES
! DANS LES BASES DE RITZ PAR APPEL A DISMOI
            zk16(ldpar(9))='MODE_STA'
            goto 12
        endif
        call rsadpa(resul, 'L', 1, 'FREQ', iorol,&
                    0, llvalo, k8bid)
        call rsadpa(resul, 'L', 1, 'AMOR_REDUIT', iorol,&
                    0, llval2, k8bid)
        zr(ldpar(2))=zr(llvalo)
        zk24(ldpar(3))=' '
        zk16(ldpar(4))=' '
        zk16(ldpar(5))='PROPRE '
        zr(ldpar(6))=omeg2
        zr(ldpar(7))=genem
        zr(ldpar(8))=genek
        if (.not.seul) then
            zk16(ldpar(9)) ='MODE_STA'
        else if (seul) then
            zk16(ldpar(9)) = 'MODE_DYN'
        endif
        zr(ldpar(10))=zr(llval2)
!
12      continue
!
! ----- INCREMENTATION DU NUMERO ORDRE
!
        iorne=iorne+1
!
10  end do
!
!
9999  continue
end subroutine
