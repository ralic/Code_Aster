subroutine lect58(ideas, nomres, mail, typres, acces,&
                  listr8, listis, precis, crit, epsi,&
                  linoch, nbnoch)
!     -----------------------------------------------------------------
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
!     LECT58 : LECTURE FICHIER FORMAT UNIVERSEL DATASET 58
!
!     IN : IDEAS : NUMERO LOGIQUE DU FICHIER UNV
!     IN : NOMRES : NOM DE LA SD RESULTATS
!     IN : MAIL : NOM DU MAILLAGE
!     IN : TYPRES : TYPE DE RESULTAT ('EVOL_ELAS','DYNA_TRANS')
!     IN : ACCES : TYPE D'ACCES ('TOUT_ORDRE','NUME_ORDRE','INST',...)
!     IN : LISTR8 : NOM DE L'OBJET CONTENANT LA LISTE DES INSTANTS
!                        OU DES FREQUENCES A LIRE
!     IN : LISTIS : NOM DE L'OBJET CONTENANT LA LISTE DES
!                        NUMEROS D'ORDRE A LIRE
!     IN : PRECIS : INDICATEUR DE VERIFICATION DE LA PRECISION
!     IN : CRIT : PRECISION : CRITERE RELATIF OU ABSOLU
!     IN : EPSI : PRECISION DEMANDEE
!     IN : LINOCH : L_K16 : LISTE DES NOMS DE CHAMP ('DEPL',...)
!     IN : NBNOCH : I     : NOMBRE DE CHAMPS A LIRE
!     -----------------------------------------------------------------
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/codent.h"
#include "asterfort/codnop.h"
#include "asterfort/decod2.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/gnomsd.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lectvl.h"
#include "asterfort/numeok.h"
#include "asterfort/reliem.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    aster_logical :: astock
    integer :: ideas, precis, nbnoch
    character(len=*) :: nomres, mail, typres, acces, listr8, listis, crit
    character(len=*) :: linoch(*)
    real(kind=8) :: epsi
!
!
!
!
!
    character(len=1) :: prfnoe
    character(len=6) :: kar
    character(len=8) :: k8bid, labk8, nomgd, licmp(30), mailla
    character(len=16) :: nomsym, motcle(1), tymocl(1)
    character(len=19) :: cns, nomch, prfchn
    character(len=24) :: vabs, vori, vcor, valmes, noojb
    character(len=80) :: ligne, repem1, rec(20)
    integer :: nbabs, itype, idir, nbnmes, ichamp, nbmesu
    integer :: vali, inoch, icham0
    integer :: label, lcorr, ibid, lori
    integer :: nbrec, ifield, nbabs1, inatur, inatu1
    integer :: lvalc, lvalr, labs
    integer :: irec, iret, ifres
    integer :: nbocc, iocc, nbno2, iagno2, i, icmpm
    integer :: numord, jcnsv, jcnsl, imes, icmp, ino, ival, jabs, ncmp
    real(kind=8) :: amin, apas, rbid, rval, dir(3)
    complex(kind=8) :: cval, czero, cun
    aster_logical :: trouve, zcmplx, ficab, ficva, vucont, vudef
!
!----------------------------------------------------------------------
!
    call jemarq()
!
    icham0 = 0
    inatu1 = 0
    nbabs1 = 0
    prfnoe='N'
!
    repem1 (  1 : 50 ) =&
     & '    -1                                            '
    repem1 ( 51 : 80 ) =&
     & '                              '
    mailla = mail
    cval = dcmplx(0.d0,0.d0)
    czero = dcmplx(0.d0,0.d0)
    cun = dcmplx(1.d0,0.d0)
    ficab = .false.
    ficva = .false.
    vabs = '&&ABSCISSES'
    vori = '&&ORIENTATIONS'
    vcor = '&&CORRESPONDANCE'
    valmes = '&&VALEURSMESUREES'
    cns = '&&CNS'
!
! RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE : NBNMES
    call dismoi('NB_NO_MAILLA', mailla, 'MAILLAGE', repi=nbnmes)
!
! VECTEUR DES NUMEROS DES NOEUDS MESURE SELON L ORDRE FICHIER UNV
    call wkvect(vcor, 'V V I', nbnmes*6, lcorr)
!
! VECTEUR DES ORIENTATIONS DE MESURE SELON L ORDRE DU FICHIER UNV
    call wkvect(vori, 'V V I', nbnmes*6, lori)
!
!  BOUCLE SUR LES CHAMPS
    do inoch = 1, nbnoch
        nbmesu = 0
        nomsym = linoch(inoch)
        if (nomsym .eq. 'SIEF_NOEU') icham0 = 2
        if (nomsym .eq. 'EPSI_NOEU') icham0 = 3
        if (nomsym .eq. 'DEPL') icham0 = 8
        if (nomsym .eq. 'VITE') icham0 = 11
        if (nomsym .eq. 'ACCE') icham0 = 12
!
        rewind ideas
!
 10     continue
        read (ideas, 1000, end = 170) ligne
        if (ligne .ne. repem1) goto 10
!
        read (ideas, '(A6)', end = 170, err = 160) kar
        if (kar .eq. '    58') then
            nbrec = 11
        else
! POSITIONNEMENT A LA FIN DU DATASET
            goto 11
        endif
!
! LECTURE DE L'ENTETE DU DATASET
        do irec = 1, nbrec
            read (ideas,'(A80)',err=160) rec(irec)
        end do
!
! RECHERCHE DU NOMBRE DE VALEURS CONTENUES DANS LE DATASET
        irec = 7
        ifield = 2
        call decod2(rec, irec, ifield, 0, nbabs,&
                    rbid, trouve)
        if (.not. ficab) then
            call wkvect(vabs, 'V V R', nbabs, labs)
            nbabs1 = nbabs
            ficab = .true.
        else
            if (nbabs .ne. nbabs1) then
                call utmess('F', 'ALGORITH4_98')
            endif
        endif
!
!- RECHERCHE DE LA NATURE DU CHAMP
!   REEL     --> INATUR = 2,4
!   COMPLEXE --> INATUR = 5,6
        ifield = 1
        call decod2(rec, irec, ifield, 0, inatur,&
                    rbid, trouve)
        if (nbmesu .eq. 0) then
            inatu1 = inatur
        else
            if (inatur .ne. inatu1) then
                call utmess('F', 'ALGORITH4_99')
            endif
        endif
        if (inatur .eq. 5 .or. inatur .eq. 6) then
            if (typres(1:6) .eq. 'DYNA_T') then
                call utmess('F', 'ALGORITH5_1')
            endif
            zcmplx = .true.
            if (.not. ficva) then
                call wkvect(valmes, 'V V C', nbabs*nbnmes*3, lvalc)
                ficva = .true.
            endif
        else
            if (typres(1:6) .eq. 'DYNA_H') then
                call utmess('F', 'ALGORITH5_2')
            endif
            zcmplx = .false.
            if (.not. ficva) then
                call wkvect(valmes, 'V V R', nbabs*nbnmes*3, lvalr)
                ficva = .true.
            endif
        endif
!
! RECUPERATION RANGEMENT DES VALEURS : EVEN / UNEVEN : ITYPE
        ifield = 3
        call decod2(rec, irec, ifield, 0, itype,&
                    rbid, trouve)
        if (itype .eq. 1) then
! RECUPERATION ABSCISSE MIN ET PAS : AMIN APAS
            ifield = 4
            call decod2(rec, irec, ifield, 1, ibid,&
                        amin, trouve)
            ifield = 5
            call decod2(rec, irec, ifield, 1, ibid,&
                        apas, trouve)
        endif
!
! LECTURE DU TYPE DU CHAMP
        irec = 9
        ifield = 1
        call decod2(rec, irec, ifield, 0, ichamp,&
                    rbid, trouve)
!
        if (ichamp .ne. icham0) goto 11
!
        if (ichamp .eq. 2) then
            if (nbmesu .eq. 0) then
                ncmp = 6
                licmp(1) = 'SIXX'
                licmp(2) = 'SIYY'
                licmp(3) = 'SIZZ'
                licmp(4) = 'SIXY'
                licmp(5) = 'SIXZ'
                licmp(6) = 'SIYZ'
                if (zcmplx) then
                    nomgd = 'SIEF_C'
                else
                    nomgd = 'SIEF_R'
                endif
            endif
        endif
        if (ichamp .eq. 3) then
            if (nbmesu .eq. 0) then
                ncmp = 6
                licmp(1) = 'EPXX'
                licmp(2) = 'EPYY'
                licmp(3) = 'EPZZ'
                licmp(4) = 'EPXY'
                licmp(5) = 'EPXZ'
                licmp(6) = 'EPYZ'
                if (zcmplx) then
                    call utmess('F', 'ALGORITH5_3')
                else
                    nomgd = 'EPSI_R'
                endif
            endif
        endif
        if (ichamp .eq. 8 .or. ichamp .eq. 11 .or. ichamp .eq. 12) then
            if (nbmesu .eq. 0) then
                ncmp = 12
                licmp(1) = 'D1'
                licmp(2) = 'D2'
                licmp(3) = 'D3'
                licmp(4) = 'D1X'
                licmp(5) = 'D1Y'
                licmp(6) = 'D1Z'
                licmp(7) = 'D2X'
                licmp(8) = 'D2Y'
                licmp(9) = 'D2Z'
                licmp(10) = 'D3X'
                licmp(11) = 'D3Y'
                licmp(12) = 'D3Z'
                if (zcmplx) then
                    nomgd = 'DEPL_C'
                else
                    nomgd = 'DEPL_R'
                endif
            endif
        endif
!
        nbmesu = nbmesu + 1
!
        if (nbmesu .gt. nbnmes*6) then
            call utmess('F', 'ALGORITH5_4')
        endif
!
! LECTURE DU NUMERO DU NOEUD
        irec = 6
        ifield = 6
        call decod2(rec, irec, ifield, 0, label,&
                    rbid, trouve)
        if (label .eq. 0) then
            ligne = rec(irec)
            labk8 = ligne(32:41)
            call jenonu(jexnom (mailla//'.NOMNOE', labk8), label)
        else
! PRE_IDEAS RAJOUTE UN 'N' DEVANT LE NUMERO DU NOEUD (VOIR ECRNEU)
            call codnop(labk8, prfnoe, 1, 1)
            call codent(label, 'G', labk8(2:8))
            call jenonu(jexnom (mailla//'.NOMNOE', labk8), label)
        endif
        zi(lcorr-1 + nbmesu) = label
!
! LECTURE DU CODE DE LA DIRECTION DE MESURE
        irec = 6
        ifield = 7
        call decod2(rec, irec, ifield, 0, idir,&
                    rbid, trouve)
        zi(lori-1 +nbmesu) = idir
!
! LECTURE DES VALEURS
        call lectvl(zcmplx, itype, nbabs, inatur, ideas,&
                    nbmesu, labs, amin, apas, lvalc,&
                    lvalr)
!
        read (ideas, 1000, end = 170) ligne
        if (ligne .ne. repem1) then
            vali = nbmesu
            call utmess('F', 'ALGORITH15_98', si=vali)
        endif
!
        goto 10
!
160     continue
! EN CAS D ERREUR DE LECTURE DU FICHIER UNV
        call utmess('F', 'ALGORITH5_5')
!
 11     continue
! POSITIONNEMENT A LA FIN DU DATASET
        read ( ideas , 1000 , end = 170 ) ligne
        if (ligne .ne. repem1) goto 11
        goto 10
!
170     continue
! FIN LECTURE FICHIER UNV
!
        ifres = iunifi ('MESSAGE')
        write(ifres,1001) nomsym,nbmesu
        1001 format('NOM_CHAM : ',a16,'NOMBRE DE MESURES : ',i6)
        if (nbmesu .eq. 0) then
            write(ifres,1002) nomsym
            1002  format('AUCUN CHAMP ',a16,' TROUVE')
            call utmess('A', 'ALGORITH5_6')
            goto 999
        endif
!
! CREATION DE SD_RESULTAT DYNA_TRANS / DYNA_HARMO / HARM_GENE : TYPRES
        if ((zcmplx) .and. (typres(1:6) .eq. 'DYNA_T')) then
            call utmess('F', 'ALGORITH5_1')
        endif
!
        if ((.not.zcmplx) .and. (typres(1:6) .ne. 'DYNA_T')) then
            call utmess('F', 'ALGORITH5_2')
        endif
!
        if (inoch .eq. 1) call rsagsd(nomres, nbabs)
        noojb='12345678.00000.NUME.PRNO'
        call gnomsd(' ', noojb, 10, 14)
        prfchn=noojb(1:19)
!
        vudef = .false.
        vucont = .false.
        do numord = 1, nbabs
            rval = zr(labs-1 +numord)
            call numeok(acces, numord, rval, listr8, listis,&
                        precis, crit, epsi, astock)
            if (astock) then
                call cnscre(mailla, nomgd, ncmp, licmp, 'V',&
                            cns)
                call jeveuo(cns//'.CNSV', 'E', jcnsv)
                call jeveuo(cns//'.CNSL', 'E', jcnsl)
                do imes = 1, nbmesu
                    icmp = zi(lori-1 + imes)
                    ival = nbabs*(imes-1) + numord
                    ino = zi(lcorr-1 + imes)
                    if (zcmplx) then
                        cval = zc(lvalc-1 +ival)
                    else
                        rval = zr(lvalr-1 +ival)
                    endif
                    if (icmp .lt. 0) then
                        icmp = -icmp
                        if (zcmplx) then
                            cval = -cval
                        else
                            rval = -rval
                        endif
                    endif
                    if (nomgd(1:4) .eq. 'DEPL') then
! ON SUPPOSE QUE ICMP EST COMPRIS ENTRE -3 ET 3
                        idir = (ino-1)*ncmp + (icmp-1)*3 + 3
                        if (zcmplx) then
                            zc(jcnsv-1 + (ino-1)*ncmp+icmp) = cval
                            zc(jcnsv-1 + idir+1) = czero
                            zc(jcnsv-1 + idir+2) = czero
                            zc(jcnsv-1 + idir+3) = czero
                            zc(jcnsv-1 + idir+icmp) = cun
                        else
                            zr(jcnsv-1 + (ino-1)*ncmp+icmp) = rval
                            zr(jcnsv-1 + idir+1) = 0.d0
                            zr(jcnsv-1 + idir+2) = 0.d0
                            zr(jcnsv-1 + idir+3) = 0.d0
                            zr(jcnsv-1 + idir+icmp) = 1.d0
                        endif
                        zl(jcnsl-1 + (ino-1)*ncmp+icmp) = .true.
                        zl(jcnsl-1 + idir+1) = .true.
                        zl(jcnsl-1 + idir+2) = .true.
                        zl(jcnsl-1 + idir+3) = .true.
!
! TRAITEMENT DES ORIENTATIONS POUR DEPL
                        call getfac('REDEFI_ORIENT', nbocc)
                        if (nbocc .gt. 0) then
                            do iocc = 1, nbocc
                                motcle(1) = 'NOEUD'
                                tymocl(1) = 'NOEUD'
                                call reliem(' ', mailla, 'NU_NOEUD', 'REDEFI_ORIENT', iocc,&
                                            1, motcle, tymocl, '&&DEFDIR', nbno2)
                                call jeveuo('&&DEFDIR', 'L', iagno2)
                                do i = 1, nbno2
                                    if (zi(iagno2-1 +i) .eq. ino) then
                                        call getvis('REDEFI_ORIENT', 'CODE_DIR', iocc=iocc,&
                                                    scal=icmpm, nbret=ibid)
                                        if (icmp .eq. icmpm) then
                                            call getvr8('REDEFI_ORIENT', 'DIRECTION', iocc=iocc,&
                                                        nbval=3, vect=dir, nbret=ibid)
                                            if (zcmplx) then
                                                zc(jcnsv-1 + idir+1) = dcmplx( dir(1),0.d0)
                                                zc(jcnsv-1 + idir+2) = dcmplx( dir(2),0.d0)
                                                zc(jcnsv-1 + idir+3) = dcmplx( dir(3),0.d0)
                                            else
                                                zr(jcnsv-1 + idir+1) = dir(1)
                                                zr(jcnsv-1 + idir+2) = dir(2)
                                                zr(jcnsv-1 + idir+3) = dir(3)
                                            endif
                                        endif
                                    endif
                                end do
                                call jedetr('&&DEFDIR')
                            end do
                        endif
! FIN TRAITEMENT DES ORIENTATIONS POUR DEPL
                    endif
!
                    if (nomgd(1:4) .eq. 'SIEF') then
                        call getfac('REDEFI_ORIENT', nbocc)
                        if ((nbocc.gt.0) .and. (.not. vucont)) then
                            call utmess('A', 'ALGORITH5_9')
                            vucont = .true.
                        endif
                        if (zcmplx) then
                            zc(jcnsv-1 + (ino-1)*ncmp+icmp) = cval
                        else
                            zr(jcnsv-1 + (ino-1)*ncmp+icmp) = rval
                        endif
                        zl(jcnsl-1 + (ino-1)*ncmp+icmp) = .true.
                    endif
!
                    if (nomgd(1:4) .eq. 'EPSI') then
                        call getfac('REDEFI_ORIENT', nbocc)
                        if ((nbocc.gt.0) .and. (.not. vudef)) then
                            call utmess('A', 'ALGORITH5_10')
                            vudef = .true.
                        endif
                        zr(jcnsv-1 + (ino-1)*ncmp+icmp) = rval
                        zl(jcnsl-1 + (ino-1)*ncmp+icmp) = .true.
                    endif
                end do
!
! RECUPERATION DU NOM DU CHAMP POUR NUMORD : NOMCH
                call rsexch(' ', nomres, nomsym, numord, nomch,&
                            iret)
                call cnscno(cns, prfchn, 'NON', 'G', nomch,&
                            'F', ibid)
!
                call rsnoch(nomres, nomsym, numord)
                if (zcmplx) then
                    call rsadpa(nomres, 'E', 1, 'FREQ', numord,&
                                0, sjv=jabs, styp=k8bid)
                else
                    call rsadpa(nomres, 'E', 1, 'INST', numord,&
                                0, sjv=jabs, styp=k8bid)
                endif
                zr(jabs) = zr(labs-1 + numord)
!
                call detrsd('CHAM_NO_S', cns)
            endif
! FIN BOUCLE SUR NUMERO ORDRE
        end do
!
999     continue
!
! FIN BOUCLE SUR LES CHAMPS DEMANDES
    end do
!
    call jedetr(vabs)
    call jedetr(vori)
    call jedetr(vcor)
    call jedetr(valmes)
!
    call jedema()
!
    1000 format ( a80 )
!
end subroutine
