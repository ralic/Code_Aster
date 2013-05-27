subroutine ecresu(resin, vectot, nbva, grand, resou,&
                  ier)
    implicit none
    include 'jeveux.h'
    include 'asterc/gettco.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelibe.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mdallo.h'
    include 'asterfort/mdarch.h'
    include 'asterfort/mdarnl.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsnoch.h'
    include 'asterfort/vtcreb.h'
    include 'asterfort/vtcrem.h'
    include 'asterfort/wkvect.h'
    integer :: npara, nbva
    character(len=*) :: resin, resou, grand
    character(len=19) :: vectot
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     REALISATION N.GREFFET
!     OPERATEUR "ECRIRE RESULTAT"
!     IN:
!       RESIN : SD_RESULTAT INITIALE HARMONIQUE
!               (VENANT DE DYNA_LINE_HARM)
!       NPARA : POINTEUR DU TABLEAU DE DONNEE VENANT DE LA FFT
!       NBVA  : NOMBRE D'INSTANTS
!       GRAND : GRANDEUR PHYSIQUE (DEPL, VITE, ACCE)
!
!     OUT:
!       RESOU   : SD_RESULTAT FINALE TRANSITOIRE
!
!
!
!
!
!     ------------------------------------------------------------------
    integer :: nbordr, ltps, jordr, ibid, i, nbsym, iarg
    integer :: ltps2, ieq, ier, neq, lval, lvals, jrefe, iret, nbva2
    integer :: nbsauv, iarchi, isto1, isto2, isto3, isto4
    integer :: jdeps, jvits, jaccs, jpass, jinst
    integer :: jfcho, jdcho, jvcho, jicho, jredc, jredd, jrevc, jrevd
    integer :: ires, n1, jdesc, nbmode, lvalv, lvala, j, lv1, lv2, lv3
    integer :: jrefam, jvint, jfreq
    real(kind=8) :: r1, rbid
    real(kind=8) :: dt
    character(len=1) :: k1b, ktyp
    character(len=4) :: grande, k4bid, nomsym(3)
    character(len=8) :: k8b
    character(len=8) :: masgen, riggen, amogen, basemo
    character(len=16) :: typout
    character(len=19) :: chdep, chdeps, krefe
    character(len=24) :: typres, chdep2, refe
    character(len=24) :: raide
    complex(kind=8) :: r1c
!
    data  refe  /'                   .REFD'/
!     ------------------------------------------------------------------
    call jemarq()
    grande = grand
    call jeveuo(vectot, 'L', npara)
    ier = 0
!   Recuperation type RESU
    call gettco(resin, typres)
    if (typres(1:10) .eq. 'DYNA_HARMO') then
        typout='DYNA_TRANS'
        nbva2=nbva
    else if (typres(1:9).eq.'HARM_GENE') then
        typout='TRAN_GENE'
        nbva2=nbva
    else if (typres(1:10).eq.'DYNA_TRANS') then
        typout='DYNA_HARMO'
        nbva2=2*nbva
    else if (typres(1:9).eq.'TRAN_GENE') then
        typout='HARM_GENE'
        nbva2=2*nbva
        call jeveuo(resin(1:8)//'           .REFD', 'L', jrefe)
        riggen = zk24(jrefe)(1:8)
        masgen = zk24(jrefe+1)(1:8)
        amogen = zk24(jrefe+2)(1:8)
    endif
!
!  Creation objet de stockage en LTPS pour les valeurs d'instants
!
!  Champs
    if (typres(6:9) .ne. 'GENE') then
        call rsexch('F', resin, grande, 1, chdep,&
                    iret)
        call jeveuo(chdep//'.VALE', 'L', lval)
!  Nombre d'equations : NEQ
        chdep2 = chdep(1:19)//'.VALE'
        call jelira(chdep2, 'LONMAX', neq, k1b)
    else
        call jelira(resin(1:19)//'.ORDR', 'LONUTI', nbordr, k1b)
        call jeveuo(resin(1:19)//'.'//grande, 'L', lval)
        chdep2 = resin(1:19)//'.'//grande
        call jelira(chdep2, 'LONMAX', neq, k1b)
        neq = neq / nbordr
    endif
    nbordr = nbva
    call wkvect('&&ECRESU.PARAMACC', 'V V R', nbva, ltps)
!
!  Creation objet resultat en sortie si non existence
!
!      NBORDR = NBVA
    call jeexin(resou(1:8)//'           .DESC', ires)
    if ((ires.eq.0) .and. (typout(6:9).ne.'GENE')) call rscrsd('G', resou, typout, nbordr)
!
    refe(1:8) = resin
    call jeveuo(refe, 'L', jrefe)
    raide = zk24(jrefe)
!
    if (typout(1:10) .eq. 'DYNA_HARMO') then
!        --- CAS OU RESULTAT EST HARMO SUR BASE PHYSIQUE
!        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
        do 10 i = 0, nbva-1
!           --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
            zr(ltps+i) = dble(zc(npara+(neq*nbva)+i))
10      continue
!
!        --- BOUCLE SUR LES FREQUENCES A SAUVEGARDER (NUM ORDRE RESU)
        do 20 i = 0, nbordr-1
            call rsadpa(resou, 'E', 1, 'FREQ', i+1,&
                        0, ltps2, k8b)
            zr(ltps2) = zr(ltps+i)
            call rsexch(' ', resou, grande, i+1, chdeps,&
                        iret)
!
            if (raide(1:1) .ne. ' ') then
!              --- CREATION D'UNE STRUCTURE CHAM_NO BASEE SUR MATRICE
!                  DE RAIDEUR
                call vtcrem(chdeps, raide, 'G', 'C')
            else
!              --- CREATION D'UNE STRUCTURE CHAM_NO "CHAMP" BASEE
!                  SUR BASE MODALE (.REFD[3])
                call vtcreb(chdeps, zk24(jrefe+3), 'G', 'C', n1)
                call assert(n1.eq.neq)
            endif
!           -------------------------------------------------------
!
!           --- REMPLIR LE .VALE PAR LES RESULTATS DANS LA TABLE FFT
            call jeveuo(chdeps//'.VALE', 'E', lvals)
            do 30 ieq = 0, neq-1
                zc(lvals+ieq) = zc(npara+nbva*ieq+i)
30          continue
            call rsnoch(resou, grande, i+1)
20      continue
    else if (typout(1:10).eq.'DYNA_TRANS') then
        do 100 i = 1, nbva
            zr(ltps+i-1) = zr(npara+(neq*nbva2)+i-1)
100      continue
        do 200 i = 1, nbordr
!  Temps
            call rsadpa(resou, 'E', 1, 'INST', (i-1),&
                        0, ltps2, k8b)
            zr(ltps2) = zr(ltps+i-1)
            call rsexch(' ', resou, grande, (i-1), chdeps,&
                        iret)
            if (raide(1:1) .ne. ' ') then
                call vtcrem(chdeps, raide, 'G', 'R')
            else
                call vtcreb(chdeps, zk24(jrefe+3), 'G', 'R', n1)
                call assert(n1.eq.neq)
            endif
!
            call jeveuo(chdeps//'.VALE', 'E', lvals)
            call jelira(chdeps//'.VALE', 'LONMAX', n1, k1b)
            call assert(n1.eq.neq)
            call jelira(chdeps//'.VALE', 'TYPE', ibid, ktyp)
            call assert(ktyp.eq.'R')
            do 300 ieq = 1, neq
                r1 = zr(npara+nbva*(ieq-1)+i-1)
                zr(lvals+ieq-1) = r1
300          continue
            call rsnoch(resou, grande, (i-1))
200      continue
    else if (typout(1:9).eq.'TRAN_GENE') then
!        --- SI NOUVEAU RESULTAT TRAN_GENE
        if (ires .eq. 0) then
!           --- BOUCLE SUR LES INSTANTS A ARCHIVER
            do 400 i = 0, nbva-1
!              --- INSTANTS SAUVEGARDEES DANS LA TABLE FFT MAIS
!                  DECALEES PAR (NEQ*NBVA2)
                zr(ltps+i) = zr(npara+(neq*nbva2)+i)
400          continue
!           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            isto1 = 0
            isto2 = 0
            isto3 = 0
            isto4 = 0
!
            jvint = 1
!           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
!               HARM_GENE A TRAN_GENE
            riggen = zk24(jrefe)(1:8)
            masgen = zk24(jrefe+1)(1:8)
            amogen = zk24(jrefe+2)(1:8)
!
            nbsauv = nbordr
!           --- RECUPERATION DU PAS DE TEMPS, NOMBRE DE MODES ET
!               ENFIN LA BASE MODALE
            dt = zr(ltps+1) - zr(ltps)
            call jeveuo(masgen(1:8)//'           .DESC', 'L', jdesc)
            nbmode = zi(jdesc+1)
            call jeveuo(masgen(1:8)//'           .REFA', 'L', jrefam)
            basemo = zk24(jrefam)(1:8)
!
            k8b = '        '
!
!           --- ALLOCATION DE LA SD DYNA_GENE RESULTAT
            call mdallo(resou(1:8), basemo, masgen, riggen, amogen,&
                        nbmode, dt, nbsauv, 0, k8b,&
                        k8b, 0, k8b, 0, k8b,&
                        jdeps, jvits, jaccs, jpass, jordr,&
                        jinst, jfcho, jdcho, jvcho, jicho,&
                        jredc, jredd, jrevc, jrevd, 'EULER           ',&
                        ibid, k4bid, 'TRAN', 'GLOB')
!
!           --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
            call wkvect('&&ECRESU.DEPL', 'V V R', neq, lvals)
            call wkvect('&&ECRESU.VITE', 'V V R', neq, lvalv)
            call wkvect('&&ECRESU.ACCE', 'V V R', neq, lvala)
            lv1 = lvals
            lv2 = lvalv
            lv3 = lvala
!           --- SWITCHER L'ORDRE DE LV(X) SELON LE TYPE DE GRANDEUR
            if (grande .eq. 'VITE') then
                lv1 = lvalv
                lv2 = lvals
            else if (grande.eq.'ACCE') then
                lv1 = lvala
                lv3 = lvals
            endif
!           --- BOUCLE SUR LES INSTANTS D'ARCHIVAGE (NUM ORDRE)
            do 500 j = 0, nbordr-1
                iarchi = j
                isto1 = j
!              --- BOUCLE SUR LES MODES
                do 600 ieq = 0, neq-1
!                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                    r1 = zr(npara+nbva*ieq+j)
!                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                    zr(lv1+ieq) = r1
                    zr(lv2+ieq) = 0.d0
                    zr(lv3+ieq) = 0.d0
600              continue
!
!              --- ARCHIVER LES RESULTATS POUR L'INSTANT EN COURS
                call mdarnl(isto1, iarchi, zr(ltps+j), dt, neq,&
                            zr(lvals), zr(lvalv), zr(lvala), isto2, 0,&
                            0.d0, 0, isto3, 0, 0.d0,&
                            0, isto4, 0, 0.d0, 0,&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevd))
500          continue
        else
!           --- SI LE RESULTAT TRAN_GENE N'EST PAS UN NOUVEAU CONCEPT,
!               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
!               (FONCTION AVEC APPELS RECURSIFS)
            call jeveuo(resou(1:8)//'           .'//grande, 'E', lvals)
            call jelira(resou(1:8)//'           .'//grande, 'LONMAX', ibid, k1b)
            do 700 j = 0, nbordr-1
                iarchi = j
                isto1 = j
                do 800 ieq = 0, neq-1
                    r1 = zr(npara+nbva*ieq+j)
                    zr(lvals+(neq*isto1)+ieq) = r1
800              continue
700          continue
        endif
    else if (typout(1:9).eq.'HARM_GENE') then
!
!        --- CAS OU LE RESULTAT EST HARMO SUR BASE GENERALISEE
!        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
        if (ires .eq. 0) then
!
            do 11 i = 0, nbva-1
!             --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
                zr(ltps+i) = dble(zc(npara+(neq*nbva)+i))
11          continue
!
!           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            isto1 = 0
!
!           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
!               TRAN_GENE A HARM_GENE
            riggen = zk24(jrefe)(1:8)
            masgen = zk24(jrefe+1)(1:8)
            amogen = zk24(jrefe+2)(1:8)
!
            nbsauv = nbordr
!           --- RECUPERATION DU NOMBRE DE MODES ET LA BASE MODALE
            call jeveuo(masgen(1:8)//'           .DESC', 'L', jdesc)
            nbmode = zi(jdesc+1)
            call jeveuo(masgen(1:8)//'           .REFA', 'L', jrefam)
            basemo = zk24(jrefam)(1:8)
!
            k8b = '        '
!           --- ALLOCATION DE LA SD DYNA_GENE RESULTAT
!           ON RECHERCHE LES CHAMPS A REMPLIR POUR LE CAS HARMONIQUE
            call getvtx(' ', 'NOM_CHAM', 1, iarg, 3,&
                        nomsym, nbsym)
            if (nbsym .eq. 0) then
                nbsym = 3
                nomsym(1) = 'DEPL'
                nomsym(2) = 'VITE'
                nomsym(3) = 'ACCE'
            endif
!
            call mdallo(resou(1:8), basemo, masgen, riggen, amogen,&
                        nbmode, rbid, nbsauv, 0, k8b,&
                        k8b, 0, k8b, 0, k8b,&
                        jdeps, jvits, jaccs, jpass, jordr,&
                        jfreq, ibid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, ibid, 'EULER           ',&
                        nbsym, nomsym, 'HARM', 'GLOB')
!
!           --- CREATION DES VECTEURS DE TRAVAIL TEMPORAIRES
            call wkvect('&&ECRESU.DEPLC', 'V V C', neq, lvals)
            call wkvect('&&ECRESU.VITEC', 'V V C', neq, lvalv)
            call wkvect('&&ECRESU.ACCEC', 'V V C', neq, lvala)
            lv1 = lvals
            lv2 = lvalv
            lv3 = lvala
!           --- SWITCHER L'ORDRE DE LV(X) SELON LE TYPE DE GRANDEUR
            if (grande .eq. 'VITE') then
                lv1 = lvalv
                lv2 = lvals
            else if (grande.eq.'ACCE') then
                lv1 = lvala
                lv3 = lvals
            endif
!
!           --- BOUCLE SUR LES FREQUENCES A SAUVEGARDER (NUM ORDRE RESU)
            do 21 j = 0, nbordr-1
                iarchi = j
                isto1 = j
!              --- BOUCLE SUR LES MODES
                do 22 ieq = 0, neq-1
!                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                    r1c = zc(npara+nbva*ieq+j)
!                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                    zc(lv1+ieq) = r1c
                    zc(lv2+ieq) = 0.d0
                    zc(lv3+ieq) = 0.d0
22              continue
                nbsym = 3
                nomsym(1) = 'DEPL'
                nomsym(2) = 'VITE'
                nomsym(3) = 'ACCE'
!
!              --- ARCHIVER LES RESULTATS POUR LA FREQUENCE EN COURS
                call mdarch(isto1, iarchi, zr(ltps+j), rbid, neq,&
                            'HARM', nbsym, nomsym, rbid, rbid,&
                            rbid, rbid, rbid, rbid, zc(lvals),&
                            zc(lvalv), zc(lvala), zc(jdeps), zc(jvits), zc(jaccs),&
                            rbid, zi(jordr), zr(jfreq))
21          continue
        else
!           --- SI LE RESULTAT HARM_GENE N'EST PAS UN NOUVEAU CONCEPT,
!               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
!               (FONCTION AVEC APPELS RECURSIFS)
            call jeveuo(resou(1:8)//'           .'//grande, 'E', lvals)
            call jelira(resou(1:8)//'           .'//grande, 'LONMAX', ibid, k1b)
            do 23 j = 0, nbordr-1
                iarchi = j
                isto1 = j
                do 24 ieq = 0, neq-1
                    r1c = zc(npara+nbva*ieq+j)
                    zc(lvals+(neq*isto1)+ieq) = r1c
24              continue
23          continue
        endif
    endif
!
!     --- FINALISER LE .REFD POUR LES CAS AVEC RESU SUR BASE PHYSIQUE
    if ((ires.eq.0) .and. (typout(6:9).ne.'GENE')) then
        krefe = resou(1:8)
        call wkvect(krefe//'.REFD', 'G V K24', 7, jordr)
        zk24(jordr) = zk24(jrefe)
        zk24(jordr+1) = zk24(jrefe+1)
        zk24(jordr+2) = zk24(jrefe+2)
        zk24(jordr+3) = zk24(jrefe+3)
        zk24(jordr+4) = zk24(jrefe+4)
        zk24(jordr+5) = zk24(jrefe+5)
        zk24(jordr+6) = zk24(jrefe+6)
        call jelibe(krefe//'.REFD')
    endif
!
    call jedetr('&&ECRESU.PARAMACC')
    call jedema()
end subroutine
