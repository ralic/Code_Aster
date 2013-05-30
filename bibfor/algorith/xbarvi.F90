subroutine xbarvi(noma, nomo, fiss, faclon, ainter)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/cesexi.h'
    include 'asterfort/conare.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexatr.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    include 'asterfort/xxmmvd.h'
    character(len=8) :: noma, nomo, fiss
    character(len=19) :: faclon, ainter
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
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
! C'EST ICI QUE L'ON REMPLIT LA 5EME COMPOSANTES DE TOPOFAC.AI
! DANS L'ELEMENT DE CONTACT XFEM
!
! CETTE COMPOSANTE VAUT :
!       1 SI L'ARETE INTERSECTÉE EST VITALE
!       0 SINON
!
! ON CRÉE AUSSI LA SD FISS(1:8)//'.CONTACT.CNCTE QUI LISTE ET RENSEIGNE
! LES ARETES APARTENANTES A UN GROUPE D'ARETES VITALES CONNECTÉES,
! IL Y A 4 COMPOSANTES PAR ARETES :
!      1 : NUMERO D'ARETE DANS LE GROUPE
!      2 : NUMERO DE GROUPE
!      3 : NUMERO DE MAILLE
!      4 : NUMERO LOCAL DE L'ARETE DANS LA MAILLE
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
!
! ----------------------------------------------------------------------
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  FISS   : NOM DE LA FISSURE EN COURS
! IN FACLON  : TOPOFAC.LO SIMPLIFIÉ
! IN/OUT AINTER : TOPOFAC.AI SIMPLIFIÉ
!
!
!
!
    integer :: jcsd1, jcsl1, jcsv1, jcsd2, jcsl2, jcsv2, jconx1, jconx2
    integer :: jlis1, ima, jma, pin, iad, iret, ninter, ifiss
    integer :: ar(12, 3), i, ia, nbar, nloc(2), nglo(2), nuno(2), neq, no
    character(len=24) :: grp(3)
    character(len=19) :: nliseq
    character(len=8) :: typma, k8bid
    integer :: zxain, in, jn, iac, ier, ncta, ncte
    integer :: jcntan, jcntes, jcnte2, jarcon, narcon, jnbpt
    logical :: lmulti, lconne
    integer :: kk, jgrp, ienr, nmaenr
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    zxain = xxmmvd('ZXAIN')
    narcon = 0
    iac = 0
!
! --- LE MULTI-HEAVISIDE EST-IL ACTIF ?
!
    call jeexin(nomo//'.FISSNO    .CELD', ier)
    if (ier .eq. 0) then
        lmulti = .false.
        ifiss = 1
    else
        lmulti = .true.
        call jeveuo('&&XCONTA.NBSP', 'L', jnbpt)
    endif
!
! --- ON RECUPERE LA LISTE DES RELATIONS D'EGALITÉS
!
    nliseq = fiss(1:8)//'.LISEQ'
    call jeexin(nliseq, ier)
    if (ier .eq. 0) then
        neq = 0
    else
        call jeveuo(nliseq, 'L', jlis1)
        call jelira(nliseq, 'LONMAX', neq, k8bid)
    endif
!
! --- Y A T-IL DES ARETES CONNECTÉES ?
!
    call jeexin(fiss(1:8)//'.CONNECTANT', ier)
    if (ier .eq. 0) then
        lconne = .false.
        ncta = 0
    else
        lconne = .true.
        call jeveuo(fiss(1:8)//'.CONNECTANT', 'L', jcntan)
        call jelira(fiss(1:8)//'.CONNECTANT', 'LONMAX', ncta, k8bid)
        call jeveuo(fiss(1:8)//'.CONNECTES ', 'L', jcntes)
    endif
!
! --- ON RECUPERE DES INFOS GLOBALES SUR LE MAILLAGE
!
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', jma)
    call jeveuo(noma(1:8)//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma(1:8)//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
! --- ACCES AUX CHAM_ELEM_S
!
    call jeveuo(faclon//'.CESD', 'L', jcsd1)
    call jeveuo(faclon//'.CESV', 'L', jcsv1)
    call jeveuo(faclon//'.CESL', 'L', jcsl1)
    call jeveuo(ainter//'.CESD', 'L', jcsd2)
    call jeveuo(ainter//'.CESL', 'E', jcsl2)
    call jeveuo(ainter//'.CESV', 'E', jcsv2)
!
! --- RECUPERATION DES GROUPES
!
    grp(1) = fiss//'.MAILFISS.HEAV'
    grp(2) = fiss//'.MAILFISS.CTIP'
    grp(3) = fiss//'.MAILFISS.HECT'
!
! --- PREMIÈRE PASSE POUR DIMENSIONER LE VECT DES ARETES CONNECTÉES
!
    if ((.not.lconne)) goto 41
!
! --- BOUCLE SUR LES GRP
!
    do 81 kk = 1, 3
        call jeexin(grp(kk), iret)
        if (iret .eq. 0) goto 81
        call jeveuo(grp(kk), 'L', jgrp)
        call jelira(grp(kk), 'LONMAX', nmaenr, k8bid)
!
! --- BOUCLE SUR LES MAILLES DU GROUPE
!
        do 11 ienr = 1, nmaenr
            ima = zi(jgrp-1+ienr)
            if (lmulti) ifiss = zi(jnbpt-1+ima)
            if (ifiss .eq. 0) goto 11
            call cesexi('C', jcsd1, jcsl1, ima, 1,&
                        ifiss, 1, iad)
            if (iad .eq. 0) goto 11
            call assert(iad.gt.0)
            ninter = zi(jcsv1-1+iad)
            if (ninter .eq. 0) goto 11
            call assert(ninter.gt.0)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jma-1+ima)), typma)
            call conare(typma, ar, nbar)
            do 21 pin = 1, ninter
!
! --- NUMERO DE L'ARETE INTERSECTÉ
!
                call cesexi('S', jcsd2, jcsl2, ima, 1,&
                            ifiss, zxain*(pin-1)+ 1, iad)
                call assert(iad.gt.0)
                ia=nint(zr(jcsv2-1+iad))
                if (ia .eq. 0) goto 21
                call assert(ia.gt.0)
!
! --- S IL S'AGIT D'UNE ARRETE, RECUP DES NUM GLOBAUX DE SES NOEUDS
!
                do 31 i = 1, 2
                    nloc(i) = ar(ia,i)
                    nglo(i) = zi(jconx1-1+zi(jconx2+ima-1)+nloc(i)-1)
31              continue
!
! --- COMPARAISON AVEC LES NOEUDS DE LA LISTE DE NOEUDS CONNECTANTS
!
                do 51 in = 1, ncta/3
                    nuno(1) = zi(jcntan-1+3*(in-1)+1)
                    if (nuno(1) .eq. nglo(1) .or. nuno(1) .eq. nglo(2)) then
                        ncte = zi(jcntan-1+3*(in-1)+2)
                        jcnte2 = zi(jcntan-1+3*(in-1)+3)
                        do 61 jn = 1, ncte
                            nuno(2) = zi(jcntes-1 + jcnte2 + jn)
                            if (nuno(2) .eq. nglo(1) .or. nuno(2) .eq. nglo( 2)) then
                                narcon = narcon + 1
                                goto 21
                            endif
61                      continue
                    endif
51              continue
21          continue
11      continue
81  end do
    call assert(narcon.gt.0)
    call wkvect('&&XBARVI.ARCON', 'V V I', 4*narcon, jarcon)
!
41  continue
!
! --- BOUCLE SUR LES GRP
!
    do 80 kk = 1, 3
        call jeexin(grp(kk), iret)
        if (iret .eq. 0) goto 80
        call jeveuo(grp(kk), 'L', jgrp)
        call jelira(grp(kk), 'LONMAX', nmaenr, k8bid)
!
! --- BOUCLE SUR LES MAILLES DU GROUPE
!
        do 10 ienr = 1, nmaenr
            ima = zi(jgrp-1+ienr)
            if (lmulti) ifiss = zi(jnbpt-1+ima)
            if (ifiss .eq. 0) goto 10
            call cesexi('C', jcsd1, jcsl1, ima, 1,&
                        ifiss, 1, iad)
            if (iad .eq. 0) goto 10
            call assert(iad.gt.0)
            ninter = zi(jcsv1-1+iad)
            if (ninter .eq. 0) goto 10
            call assert(ninter.gt.0)
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jma-1+ima)), typma)
            call conare(typma, ar, nbar)
            do 20 pin = 1, ninter
!
! --- NUMERO DE L'ARETE INTERSECTÉE
!
                call cesexi('S', jcsd2, jcsl2, ima, 1,&
                            ifiss, zxain*(pin-1)+ 1, iad)
                call assert(iad.gt.0)
                ia=nint(zr(jcsv2-1+iad))
                call cesexi('S', jcsd2, jcsl2, ima, 1,&
                            ifiss, zxain*(pin-1)+ 2, iad)
                call assert(iad.gt.0)
                no=nint(zr(jcsv2-1+iad))
                call assert(no.ge.0)
                call cesexi('S', jcsd2, jcsl2, ima, 1,&
                            ifiss, zxain*(pin-1)+ 5, iad)
                if (iad .le. 0) iad = -iad
!
! --- S IL S'AGIT D'UN NOEUD, ALORS IL EST VITAL
!
                if (no .gt. 0) then
                    zr(jcsv2-1+iad)=1
                    zl(jcsl2-1+iad)=.true.
                endif
                if (ia .gt. 0) then
!
! --- S IL S'AGIT D'UNE ARRETE, RECUP DES NUM GLOBAUX DE SES NOEUDS
!
                    do 30 i = 1, 2
                        nloc(i) = ar(ia,i)
                        nglo(i) = zi(jconx1-1+zi(jconx2+ima-1)+nloc(i) -1)
30                  continue
!
! --- COMPARAISON AVEC LES COUPLES DE NO DE LA LISTE DE RELAT D'EGALITÉS
!
                    do 50 i = 1, neq/2
                        nuno(1) = zi(jlis1-1+2*(i-1)+1)
                        nuno(2) = zi(jlis1-1+2*(i-1)+2)
                        if (nuno(1) .eq. nglo(1) .and. nuno(2) .eq. nglo(2) .or. nuno(1)&
                            .eq. nglo(2) .and. nuno(2) .eq. nglo(1)) then
!
! --- SI C EGAL, ON EST SUR UNE ARETE VITALE, ON MET LE STATUT À 1
!
                            zr(jcsv2-1+iad)=1
                            zl(jcsl2-1+iad)=.true.
!
! --- COMPARAISON AVEC LES NOEUDS DE LA LISTE DE NOEUDS CONNECTANTS
!
                            do 60 in = 1, ncta/3
                                nuno(1) = zi(jcntan-1+3*(in-1)+1)
                                if (nuno(1) .eq. nglo(1) .or. nuno(1) .eq. nglo(2)) then
!
! --- SI C EGALE, ON RECUPERE LA LISTE DES NOEUDS CONNECTES
!
                                    ncte = zi(jcntan-1+3*(in-1)+2)
                                    jcnte2 = zi(jcntan-1+3*(in-1)+3)
                                    do 70 jn = 1, ncte
!
! --- ON COMPARE LES NUMEROS DE CETTE LISTE À CELLE DE L'ARETE
!
                                        nuno(2) = zi(jcntes-1 + jcnte2 + jn)
                                        if (nuno(2) .eq. nglo(1) .or. nuno( 2) .eq. nglo(2)) then
!
! --- SI C EGALE, ON NOTE LE NUMERO DE MAILLE, LE NUMERO LOCAL DE
! --- L'ARETE ET ON SORT
!
                                            iac = iac + 1
                                            call assert(iac.le.narcon)
                                            zi(jarcon-1+4*(iac-1)+1) = in
                                            zi(jarcon-1+4*(iac-1)+2) =&
                                        ima
                                            zi(jarcon-1+4*(iac-1)+3) = ia
                                            zi(jarcon-1+4*(iac-1)+4) = jn
                                            goto 20
                                        endif
70                                  continue
                                endif
60                          continue
                            goto 20
                        else
                            zr(jcsv2-1+iad)=0
                            zl(jcsl2-1+iad)=.true.
                        endif
!
50                  continue
                endif
!
20          continue
10      continue
80  end do
!
    call assert(iac.eq.narcon)
    if (narcon .gt. 0) then
!
! --- ON ECRIT LE VECTEUR DES ARETES CONECTÉES
!
        call wkvect(fiss(1:8)//'.CNCTE     ', 'G V I', 4*narcon, jcntes)
        ncte = 0
!
! --- ON BOUCLE SUR TOUT LES NOEUDS CONNECTANT, CHAQUE NOEUD
! --- CONNECTANT DEFINI UN GROUPE
!
        do 90 in = 1, ncta/3
!
! --- ON BOUCLE SUR TOUTES LES ARETES CONNECTEES
!
            do 100 iac = 1, narcon
                if (zi(jarcon-1+4*(iac-1)+2) .eq. in) then
                    ncte = ncte+1
!
! --- DES QU'IL Y EN A UNE QUI CORRESPOND AU GROUPE EN COURS,
! --- ON LE STOQUE DANS CE GROUPE
!
                    zi(jcntes-1+4*(ncte-1)+1) = zi(jarcon-1+4*(iac-1)+ 4)
                    zi(jcntes-1+4*(ncte-1)+2) = in
                    zi(jcntes-1+4*(ncte-1)+3) = zi(jarcon-1+4*(iac-1)+ 2)
                    zi(jcntes-1+4*(ncte-1)+4) = zi(jarcon-1+4*(iac-1)+ 3)
                endif
100          continue
90      continue
!
! --- MENAGE
!
        call jedetr(fiss(1:8)//'.CONNECTANT')
        call jedetr(fiss(1:8)//'.CONNECTES ')
        call jedetr('&&XBARVI.ARCON')
    endif
!
    call jedema()
end subroutine
