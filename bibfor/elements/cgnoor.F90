subroutine cgnoor(mafour, nomail, motfac, iocc, nbmc,&
                  motcle, typmcl, typlig, nbma, ndorig,&
                  ndextr, typm, vecori)
    implicit   none
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/i2extf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
    integer :: iocc, nbmc, nbma
    character(len=*) :: typlig
    character(len=24) :: mafour
    character(len=8) :: nomail, ndorig, ndextr, typm
    character(len=16) :: motcle(*), typmcl(*)
    character(len=*) :: motfac
    real(kind=8) :: vecori(3)
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
!-----------------------------------------------------------------------
!  CETTE ROUTINE EST UTILISEE DANS DEFI_GROUP ET DEFI_FOND_FISS
!
! FONCTION REALISEE:
!  * CREER LA LISTE DES NUMEROS DES SEGX FORMANT UNE LIGNE (MAFOUR,NBMA)
!  * CALCULER LE NOM DES 2 NOEUDS EXTREMITES DE MAFOUR: NDORIG ET NDEXTR
!    (NDORIG EST TOUJOURS CALCULE. MAIS PARFOIS NDEXTR=' ')
!  * CALCULER LE TYPE DES SEGMENTS (SEG2/SEG3/SEG4)
!  * CALCULER LE VECTEUR D'ORIENTATION SI NECESSAIRE (VECORI)
!
! CETTE FONCTION REALISE AUSSI CERTAINES VERIFICATIONS :
!  * TOUTES LES MAILLES DE MAFOUR SONT DES SEGX
!  * TOUTES LES MAILLES DE MAFOUR SONT DE MEME TYPE SEG2/3/4
!  * MAFOUR DEFINIT UNE LIGNE CONTINUE SANS BIFURCATION
!
! ATTENTION : LA LISTE MAFOUR N'EST PAS TRIEE
!
!     ENTREES:
!        MAFOUR     : NOM DE L'OBJET QUI CONTIENDRA LES MAILLES SEG
!        NOMAIL     : NOM DU MAILLAGE
!        MOTFAC     : MOT-CLE FACTEUR
!        IOCC       : OCCURENCE COURANTE DE MOTFAC
!        NBMC       : NOMBRE DE MOT-CLE SIMPLE
!        MOTCLE     : MOT-CLE SIMPLE TYPE MA OU GROUP_MA A TRAITER
!        TYPMCL     : TYPE D'ENTITE ENTREE SOUS LE MOT-CLE
!        TYPLIG     : TYPE DE LIGNE : 'FERME' (OU NON)
!                     SI TYPLIG.EQ.' ' ET SI NDORIG=NDEXTR (/=' '),
!                     ON CONSIDERE QUE TYPLIG='FERME'
!     SORTIES:
!        MAFOUR     : L'OBJET EST ALLOUE ET REMPLI
!        NBMA       : NOMBRE DE MAILLES CONSIDEREES
!        NDORIG     : NOM DU NOEUD ORIGINE
!        NDEXTR     : NOM DU NOEUD EXTREMITE
!        TYPM       : TYPE DE MAILLE DE LA LISTE MAFOUR (SEG2/SEG3/SEG4)
!        VECORI     : 3 COORDONNEES EVENTUELLEMENT DONNEES PAR VECT_ORIE
!                     (0,0,0) SINON
!-----------------------------------------------------------------------
!
!
    integer :: jmail, jtypm, iatyma
    integer :: ier, im, n1, n2, nid, nig, nbnot
    integer :: nunori, trouv, ibid, in, nd
    integer :: existe, iret, ima
    integer :: jcour1, jcour2, jcour3, jcour4
    character(len=8) :: k8b, nomma, typmp
    character(len=16) :: k16bid, nomcmd
    character(len=24) :: conec, typp, nommai, nomnoe, mesmai, valk(2), nogrp
    logical :: bug
    integer :: iarg
! DEB-------------------------------------------------------------------
    call jemarq()
!
    call getres(k8b, k16bid, nomcmd)
!
!     ------------------------------------------------------------------
!     INITIALISATION DE VARIABLES
!     ------------------------------------------------------------------
    mesmai='&&CGNOOR.MES_MAILLES'
    conec=nomail//'.CONNEX         '
    typp=nomail//'.TYPMAIL        '
    nommai=nomail//'.NOMMAI         '
    nomnoe=nomail//'.NOMNOE         '
    call dismoi('F', 'NB_NO_MAILLA', nomail, 'MAILLAGE', nbnot,&
                k8b, ier)
    call jeveuo(typp, 'L', iatyma)
!
!
!     ------------------------------------------------------------------
!     RECUPERATION DES MAILLES SOUS LES MOT-CLES "MOTCLE"
!       => MESMAI : NOMS DES MAILLES
!       => MAFOUR : NUMEROS DES MAILLES
!     ------------------------------------------------------------------
    call reliem(' ', nomail, 'NO_MAILLE', motfac, iocc,&
                nbmc, motcle, typmcl, mesmai, nbma)
!
    if (nbma .eq. 0) call u2mess('F', 'ELEMENTS_66')
    call jeveuo(mesmai, 'L', jmail)
    call wkvect(mafour, 'V V I', nbma, jcour2)
    do 10,im=1,nbma
    call jenonu(jexnom(nommai, zk8(jmail-1+im)), ima)
    zi(jcour2-1+im)=ima
    10 end do
!
!
!     ------------------------------------------------------------------
!     --- VERIFICATION DE L'EXISTENCE DES MAILLES
!     --- VERIFICATION QUE LES MAILLES SONT TOUTES SEG2, SEG3, SEG4 :
!     ------------------------------------------------------------------
    typmp=' '
    ier=0
    do 20,im=1,nbma
    nomma=zk8(jmail-1+im)
    call jeexin(jexnom(nommai, nomma), existe)
    if (existe .eq. 0) then
        ier=ier+1
        call u2mesg('E', 'ELEMENTS5_19', 1, nomma, 1,&
                    iocc, 0, 0.d0)
    else
        call jenonu(jexnom(nommai, nomma), ibid)
        jtypm=iatyma-1+ibid
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypm)), typm)
        if (typm(1:3) .ne. 'SEG') then
            if (nomcmd .ne. 'DEFI_GROUP') call u2mess('F', 'RUPTURE0_63')
            ier=ier+1
            call u2mesg('E', 'ELEMENTS5_20', 1, nomma, 1,&
                        iocc, 0, 0.d0)
        endif
        if (im .gt. 1) then
            if (typm .ne. typmp) then
                ier=ier+1
                call u2mesg('E', 'ELEMENTS5_21', 1, nomma, 1,&
                            iocc, 0, 0.d0)
            endif
        endif
        typmp=typm
    endif
    20 end do
    if (ier .gt. 0) then
        call u2mesi('F', 'ELEMENTS5_15', 1, iocc)
    endif
!
!
! --- LECTURE DU NOM DU NOEUD ORIGINE (S'IL EST FOURNI)
    call getvtx(motfac, 'NOEUD_ORIG', iocc, iarg, 0,&
                k8b, n1)
    call getvtx(motfac, 'GROUP_NO_ORIG', iocc, iarg, 0,&
                nogrp, n2)
    if (n1 .ne. 0) then
        call getvtx(motfac, 'NOEUD_ORIG', iocc, iarg, 1,&
                    ndorig, n1)
    else if (n2.ne.0) then
        call getvtx(motfac, 'GROUP_NO_ORIG', iocc, iarg, 1,&
                    nogrp, n2)
        call utnono(' ', nomail, 'NOEUD', nogrp, ndorig,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogrp)
        else if (iret.eq.1) then
            valk(1)='GROUP_NO_ORIG'
            valk(2)=ndorig
            call u2mesk('A', 'ELEMENTS5_17', 2, valk)
        endif
    else
        ndorig=' '
    endif
!
!
! --- LECTURE DU NOM DU NOEUD EXTREMITE (S'IL EST FOURNI)
    call getvtx(motfac, 'NOEUD_EXTR', iocc, iarg, 0,&
                k8b, n1)
    call getvtx(motfac, 'GROUP_NO_EXTR', iocc, iarg, 0,&
                nogrp, n2)
    if (n1 .ne. 0) then
        call getvtx(motfac, 'NOEUD_EXTR', iocc, iarg, 1,&
                    ndextr, n1)
    else if (n2.ne.0) then
        call getvtx(motfac, 'GROUP_NO_EXTR', iocc, iarg, 1,&
                    nogrp, n2)
        call utnono(' ', nomail, 'NOEUD', nogrp, ndextr,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogrp)
        else if (iret.eq.1) then
            valk(1)='GROUP_NO_EXTR'
            valk(2)=ndextr
            call u2mesk('A', 'ELEMENTS5_17', 2, valk)
        endif
    else
        ndextr=' '
    endif
!
!
!     ------------------------------------------------------------------
!     --- CONSTRUCTION
!     --- 1 - VECTEUR DE TRAVAIL LOCAL CONTENANT LES NOEUDS EXTREMITES
!     ---     DE CHAQUE MAILLE
!     --- 2 - VECTEUR DE TRAVAIL LOCAL CONTENANT POUR CHAQUE NOEUD
!     ---     DU MAILLAGE :
!     ---     - 0 SI LE NOEUD N'APPARTIENT AUX MAILLES
!     ---     - 1 SI INTERNE (APPARTIENT A DEUX MAILLES)
!     ---     - 2 SI EXTREMITE
!     ------------------------------------------------------------------
    call wkvect('&&CGNOOR.NOEUDS_EXTREM', 'V V I', 2*nbma, jcour1)
    call wkvect('&&CGNOOR.TYPE_NOEUD', 'V V I', nbnot, jcour4)
    do 30 im = 1, nbma
        call i2extf(zi(jcour2-1+im), 1, conec(1:15), typp(1:16), nig,&
                    nid)
        zi(jcour1-1+im)=nig
        zi(jcour1-1+nbma+im)=nid
        zi(jcour4-1+nig)=zi(jcour4-1+nig)+1
        zi(jcour4-1+nid)=zi(jcour4-1+nid)+1
30  end do
!
!
! --- VERIFICATION QUE LA LIGNE EST CONTINUE ET UNIQUE
    n1=0
    n2=0
    bug=.false.
    do 40 im = 1, nbnot
!        COMPTAGE DES EXTREMITES
        if (zi(jcour4-1+im) .eq. 1) n1=n1+1
!        COMPTAGE NOEUDS APPARTENANT A PLUS DE DEUX MAILLES
        if (zi(jcour4-1+im) .gt. 2) n2=n2+1
40  end do
!     IL NE PEUT Y AVOIR QUE 2 NOEUDS EXTREMITES
    if (n1 .gt. 2) bug=.true.
!     IL NE DOIT PAS Y AVOIR DE NOEUDS APPARTENANT A PLUS DE DEUX
!     MAILLES
    if (n2 .ne. 0) bug=.true.
    if (bug) then
        call u2mess('F', 'ELEMENTS5_16')
    endif
!
!
!     -- CALCUL DE VECORI:
!     --------------------
    vecori(1)=0.d0
    vecori(2)=0.d0
    vecori(3)=0.d0
    if (nomcmd .eq. 'DEFI_GROUP' .and. motfac .eq. 'CREA_GROUP_NO') then
        call getvr8(motfac, 'VECT_ORIE', iocc, iarg, 3,&
                    vecori, n1)
        if ((ndorig.eq.ndextr) .and. (ndorig.ne.' ')) then
            if (n1 .le. 0) call u2mess('A', 'ELEMENTS_70')
        endif
    endif
!
!
!
!     ------------------------------------------------------------------
!     --- VERIFICATION DU NOEUD EXTREMITE :
!     ------------------------------------------------------------------
    if (ndextr .ne. ' ') then
        call jenonu(jexnom(nomnoe, ndextr), nunori)
!
!       ON VERIFIE QU'IL S'AGIT BIEN D'UNE EXTREMITE
        trouv=0
        do 50 im = 1, nbma
            if (zi(jcour1-1+im) .eq. nunori) trouv=trouv+1
            if (zi(jcour1-1+nbma+im) .eq. nunori) trouv=trouv+1
50      continue
!
        if (trouv .eq. 0) call u2mesk('F', 'ELEMENTS_68', 1, ndorig)
        if (typlig .eq. 'FERME') then
            if (trouv .ne. 2) call u2mesk('F', 'ELEMENTS_69', 1, ndextr)
        else
            if (.not.(typlig.eq.' '.and.ndorig.eq.ndextr)) then
                if (trouv .ne. 1) call u2mesk('F', 'ELEMENTS_69', 1, ndextr)
            endif
        endif
    endif
!
!
!
!     ------------------------------------------------------------------
!     --- VERIFICATION DU NOEUD ORIGINE :
!     ------------------------------------------------------------------
    if (ndorig .ne. ' ') then
        call jenonu(jexnom(nomnoe, ndorig), nunori)
!
!       ON VERIFIE QU'IL S'AGIT BIEN D'UNE EXTREMITE
        trouv=0
        do 51 im = 1, nbma
            if (zi(jcour1-1+im) .eq. nunori) trouv=trouv+1
            if (zi(jcour1-1+nbma+im) .eq. nunori) trouv=trouv+1
51      continue
!
        if (trouv .eq. 0) call u2mesk('F', 'ELEMENTS_68', 1, ndorig)
        if (typlig .eq. 'FERME') then
            if (trouv .ne. 2) call u2mesk('F', 'ELEMENTS_69', 1, ndorig)
        else
            if (.not.(typlig.eq.' '.and.ndorig.eq.ndextr)) then
                if (trouv .ne. 1) call u2mesk('F', 'ELEMENTS_69', 1, ndorig)
            endif
        endif
!
    else
!
!     ------------------------------------------------------------------
!     --- SI L'ORIGINE EST DONNEE
!     --- CONSTRUCTION D'UN VECTEUR DE TRAVAIL LOCAL POUR TROUVER
!     --- L'ORIGINE
!     ------------------------------------------------------------------
        call wkvect('&&CGNOOR.NOEUD_APPARIES', 'V V I', 2*nbma, jcour3)
!       LISTE DES NOEUDS DEJA APPARIES
        do 60 in = 1, nbma*2
            zi(jcour3-1+in)=0
60      continue
!
!       PARCOURS DE L'ENSEMBLE DES NOEUDS
        do 90 in = 1, nbma*2
            if (zi(jcour3-1+in) .ne. 0) goto 80
            nunori=zi(jcour1-1+in)
!
            do 70 nd = in+1, nbma*2
                if (zi(jcour1-1+nd) .eq. nunori) then
                    zi(jcour3-1+nd)=1
                    goto 80
!
                endif
70          continue
!
!         NUNORI N'APPARAIT QU'UNE FOIS : C'EST L'ORIGINE
            goto 100
!
80          continue
90      continue
        call u2mess('F', 'ELEMENTS_71')
!
100      continue
        call jenuno(jexnum(nomnoe, nunori), ndorig)
        call u2mesk('I', 'ELEMENTS_72', 1, ndorig)
        call jedetr('&&CGNOOR.NOEUD_APPARIES')
!
    endif
!
    call jedetr('&&CGNOOR.NOEUDS_EXTREM')
    call jedetr('&&CGNOOR.TYPE_NOEUD')
    call jedetr('&&CGNOOR.MES_MAILLES')
!
    call jedema()
end subroutine
