subroutine ecresu(resin, vectot, nbva, grand, resou,&
                  ier)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdallo.h"
#include "asterfort/mdarch.h"
#include "asterfort/mdarnl.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
    integer :: npara, nbva
    character(len=*) :: resin, resou, grand
    character(len=19) :: vectot
!     ------------------------------------------------------------------
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
    integer :: nbordr, ltps, jordr, ibid, i, nbsym
    integer :: ltps2, ieq, ier, neq, lval, lvals, iret, nbva2
    integer :: nbsauv, iarchi, isto1, isto2, isto3, isto4
    integer :: jdeps, jvits, jaccs, jpass, jinst
    integer :: jfcho, jdcho, jvcho, jicho, jredc, jredd, jrevc, jrevv
    integer :: ires, n1, jdesc, nbmode, lvalv, lvala, j, lv1, lv2, lv3
    integer :: jrefam, jvint, jfreq
    real(kind=8) :: r1, rbid
    real(kind=8) :: dt
    character(len=1) :: ktyp
    character(len=4) :: grande, k4bid(3), nomsym(3)
    character(len=8) :: k8b
    character(len=8) :: masgen, riggen, amogen, basemo
    character(len=16) :: typout
    character(len=19) :: chdep, chdeps
    character(len=24) :: typres, chdep2
    character(len=24) :: raide, numedd
    complex(kind=8) :: r1c
!
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
        call dismoi('F', 'REF_RIGI_PREM', resin, 'RESU_DYNA', ibid,&
                    riggen, iret)
        call dismoi('F', 'REF_MASS_PREM', resin, 'RESU_DYNA', ibid,&
                    masgen, iret)
        call dismoi('F', 'REF_AMOR_PREM', resin, 'RESU_DYNA', ibid,&
                    amogen, iret)
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
        call jelira(chdep2, 'LONMAX', neq)
    else
        call jelira(resin(1:19)//'.ORDR', 'LONUTI', nbordr)
        call jeveuo(resin(1:19)//'.'//grande, 'L', lval)
        chdep2 = resin(1:19)//'.'//grande
        call jelira(chdep2, 'LONMAX', neq)
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
    call dismoi('F', 'REF_RIGI_PREM', resin, 'RESU_DYNA', ibid,&
                raide, iret)
!
    if (typout(1:10) .eq. 'DYNA_HARMO') then
!        --- CAS OU RESULTAT EST HARMO SUR BASE PHYSIQUE
!        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
        do i = 0, nbva-1
!           --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
            zr(ltps+i) = dble(zc(npara+(neq*nbva)+i))
        end do
!
!        --- BOUCLE SUR LES FREQUENCES A SAUVEGARDER (NUM ORDRE RESU)
        do i = 0, nbordr-1
            call rsadpa(resou, 'E', 1, 'FREQ', i+1,&
                        0, sjv=ltps2, styp=k8b)
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
!
                call dismoi('F', 'NUME_DDL', resin, 'RESU_DYNA', ibid,&
                            numedd, iret)
                call vtcreb(chdeps, numedd(1:8), 'G', 'C', n1)
                ASSERT(n1.eq.neq)
            endif
!           -------------------------------------------------------
!
!           --- REMPLIR LE .VALE PAR LES RESULTATS DANS LA TABLE FFT
            call jeveuo(chdeps//'.VALE', 'E', lvals)
            do ieq = 0, neq-1
                zc(lvals+ieq) = zc(npara+nbva*ieq+i)
            end do
            call rsnoch(resou, grande, i+1)
        end do
    else if (typout(1:10).eq.'DYNA_TRANS') then
        do i = 1, nbva
            zr(ltps+i-1) = zr(npara+(neq*nbva2)+i-1)
        end do
        do i = 1, nbordr
!  Temps
            call rsadpa(resou, 'E', 1, 'INST', (i-1),&
                        0, sjv=ltps2, styp=k8b)
            zr(ltps2) = zr(ltps+i-1)
            call rsexch(' ', resou, grande, (i-1), chdeps,&
                        iret)
            if (raide(1:1) .ne. ' ') then
                call vtcrem(chdeps, raide, 'G', 'R')
            else
                call dismoi('F', 'NUME_DDL', resin, 'RESU_DYNA', ibid,&
                            numedd, iret)
                call vtcreb(chdeps, numedd(1:8), 'G', 'R', n1)
                ASSERT(n1.eq.neq)
            endif
!
            call jeveuo(chdeps//'.VALE', 'E', lvals)
            call jelira(chdeps//'.VALE', 'LONMAX', n1)
            ASSERT(n1.eq.neq)
            call jelira(chdeps//'.VALE', 'TYPE', cval=ktyp)
            ASSERT(ktyp.eq.'R')
            do ieq = 1, neq
                r1 = zr(npara+nbva*(ieq-1)+i-1)
                zr(lvals+ieq-1) = r1
            end do
            call rsnoch(resou, grande, (i-1))
        end do
    else if (typout(1:9).eq.'TRAN_GENE') then
!        --- SI NOUVEAU RESULTAT TRAN_GENE
        if (ires .eq. 0) then
!           --- BOUCLE SUR LES INSTANTS A ARCHIVER
            do i = 0, nbva-1
!              --- INSTANTS SAUVEGARDEES DANS LA TABLE FFT MAIS
!                  DECALEES PAR (NEQ*NBVA2)
                zr(ltps+i) = zr(npara+(neq*nbva2)+i)
            end do
!           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            isto1 = 0
            isto2 = 0
            isto3 = 0
            isto4 = 0
!
            jvint = 1
!           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
!               HARM_GENE A TRAN_GENE
            call dismoi('F', 'REF_RIGI_PREM', resin, 'RESU_DYNA', ibid,&
                        riggen, iret)
            call dismoi('F', 'REF_MASS_PREM', resin, 'RESU_DYNA', ibid,&
                        masgen, iret)
            call dismoi('F', 'REF_AMOR_PREM', resin, 'RESU_DYNA', ibid,&
                        amogen, iret)
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
            nbsym = 0
            call mdallo(resou(1:8), basemo, masgen, riggen, amogen,&
                        nbmode, dt, nbsauv, 0, k8b,&
                        k8b, 0, k8b, 0, k8b,&
                        jdeps, jvits, jaccs, jpass, jordr,&
                        jinst, jfcho, jdcho, jvcho, jicho,&
                        jredc, jredd, jrevc, jrevv, 'EULER           ',&
                        nbsym, k4bid, 'TRAN', 'GLOB')
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
            do j = 0, nbordr-1
                iarchi = j
                isto1 = j
!              --- BOUCLE SUR LES MODES
                do ieq = 0, neq-1
!                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                    r1 = zr(npara+nbva*ieq+j)
!                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                    zr(lv1+ieq) = r1
                    zr(lv2+ieq) = 0.d0
                    zr(lv3+ieq) = 0.d0
                end do
!
!              --- ARCHIVER LES RESULTATS POUR L'INSTANT EN COURS
                call mdarnl(isto1, iarchi, zr(ltps+j), dt, neq,&
                            zr(lvals), zr(lvalv), zr(lvala), isto2, 0,&
                            [0.d0], 0, isto3, 0, [0.d0],&
                            [0], isto4, 0, [0.d0], [0],&
                            zr(jdeps), zr(jvits), zr(jaccs), zr( jpass), zi(jordr),&
                            zr(jinst), zr(jfcho), zr(jdcho), zr( jvcho), zi(jicho),&
                            zr(jvint), zi(jredc), zr(jredd), zi( jrevc), zr(jrevv))
            end do
        else
!           --- SI LE RESULTAT TRAN_GENE N'EST PAS UN NOUVEAU CONCEPT,
!               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
!               (FONCTION AVEC APPELS RECURSIFS)
            call jeveuo(resou(1:8)//'           .'//grande, 'E', lvals)
            call jelira(resou(1:8)//'           .'//grande, 'LONMAX', ibid)
            do j = 0, nbordr-1
                iarchi = j
                isto1 = j
                do ieq = 0, neq-1
                    r1 = zr(npara+nbva*ieq+j)
                    zr(lvals+(neq*isto1)+ieq) = r1
                end do
            end do
        endif
    else if (typout(1:9).eq.'HARM_GENE') then
!
!        --- CAS OU LE RESULTAT EST HARMO SUR BASE GENERALISEE
!        --- RECUPERER LA LISTE DES FREQUENCES DE LA TABLE FFT
        if (ires .eq. 0) then
!
            do i = 0, nbva-1
!             --- LES FREQUENCES SONT DECALEES PAR (NEQ*NBVA) DS LA TBLE
                zr(ltps+i) = dble(zc(npara+(neq*nbva)+i))
            end do
!
!           --- INITIALISATION DES INDICES D'ARCHIVAGE POUR MDARCH
            isto1 = 0
!
!           --- INFORMATIONS POUR LE .REFD TRANSMIS DIRECTEMENT DE
!               TRAN_GENE A HARM_GENE
            call dismoi('F', 'REF_RIGI_PREM', resin, 'RESU_DYNA', ibid,&
                        riggen, iret)
            call dismoi('F', 'REF_MASS_PREM', resin, 'RESU_DYNA', ibid,&
                        masgen, iret)
            call dismoi('F', 'REF_AMOR_PREM', resin, 'RESU_DYNA', ibid,&
                        amogen, iret)
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
            call getvtx(' ', 'NOM_CHAM', nbval=3, vect=nomsym, nbret=nbsym)
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
            do j = 0, nbordr-1
                iarchi = j
                isto1 = j
!              --- BOUCLE SUR LES MODES
                do ieq = 0, neq-1
!                 --- RECUPERER LA VALEUR DANS LA TABLE FFT
                    r1c = zc(npara+nbva*ieq+j)
!                 --- SAUVER LA VALEUR DANS LE VECT DE TRAVAIL ASSOCIE
                    zc(lv1+ieq) = r1c
                    zc(lv2+ieq) = 0.d0
                    zc(lv3+ieq) = 0.d0
                end do
                nbsym = 3
                nomsym(1) = 'DEPL'
                nomsym(2) = 'VITE'
                nomsym(3) = 'ACCE'
!
!              --- ARCHIVER LES RESULTATS POUR LA FREQUENCE EN COURS
                call mdarch(isto1, iarchi, zr(ltps+j), 0.d0, neq,&
                            'HARM', nbsym, nomsym, [0.d0], [0.d0],&
                            [0.d0], [0.d0], [0.d0], [0.d0], zc(lvals),&
                            zc(lvalv), zc(lvala), zc(jdeps), zc(jvits), zc(jaccs),&
                            [0.d0], zi(jordr), zr(jfreq))
            end do
        else
!           --- SI LE RESULTAT HARM_GENE N'EST PAS UN NOUVEAU CONCEPT,
!               ALORS MODIFIER DIRECTEMENT LES VALEURS DANS LA SD
!               (FONCTION AVEC APPELS RECURSIFS)
            call jeveuo(resou(1:8)//'           .'//grande, 'E', lvals)
            call jelira(resou(1:8)//'           .'//grande, 'LONMAX', ibid)
            do j = 0, nbordr-1
                iarchi = j
                isto1 = j
                do ieq = 0, neq-1
                    r1c = zc(npara+nbva*ieq+j)
                    zc(lvals+(neq*isto1)+ieq) = r1c
                end do
            end do
        endif
    endif
!
!     --- FINALISER LE .REFD POUR LES CAS AVEC RESU SUR BASE PHYSIQUE
    if ((ires.eq.0) .and. (typout(6:9).ne.'GENE')) then
        call refdcp(resin, resou)
    endif
!
    call jedetr('&&ECRESU.PARAMACC')
    call jedema()
end subroutine
