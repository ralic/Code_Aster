subroutine cfimp4(ds_contact, mesh, ifm)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apinfi.h"
#include "asterfort/apnomp.h"
#include "asterfort/apvect.h"
#include "asterfort/assert.h"
#include "asterfort/cfcald.h"
#include "asterfort/cfconn.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnoap.h"
#include "asterfort/cfnomm.h"
#include "asterfort/cftypn.h"
#include "asterfort/cfzonn.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmnorm.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer, intent(in) :: ifm
    character(len=8), intent(in) :: mesh
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! IMPRESSION DES INFOS DETAILLES DE L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
! IN  IFM    : UNITE D'IMPRESSION DU MESSAGE
! In  mesh             : name of mesh
! In  ds_contact       : datastructure for contact management
!
    integer :: ztacf
    character(len=24) :: tacfin
    integer :: jtacf
    character(len=24) :: apcoef, tangco
    integer :: japcoe, jtango
    character(len=24) :: apcofr
    integer :: japcof
    character(len=24) :: jeusup, jeuite
    integer :: jjeusu, jjeuit
    character(len=24) :: appoin, numlia
    integer :: japptr, jnumli
    character(len=24) :: nbddl, apddl
    integer :: jnbddl, japddl
    integer :: typapp, entapp
    integer :: nzoco, ntnoe, nbliai, nnoco, ndimg
    integer :: k, izone, ino, iliai, iddle, inom, iddlm, ip
    integer :: posno, posnoe, posmam
    character(len=8) :: nomno, nomnoe, nommam, nomnom, nomno2, nomapp
    integer :: posno2
    integer :: jdece, jdecm, jdecno
    integer :: nbddlt, nbddle, nbddlm, nbnom
    character(len=4) :: typno, type2
    character(len=19) :: sdappa
    character(len=16) :: nompt
    real(kind=8) :: jeuold, dissup
    real(kind=8) :: coefff, coefpn, coefpt
    real(kind=8) :: tau1(3), tau2(3), norm(3)
    aster_logical :: lnodal, lctfd, lfrot
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INFOS SUR LA CHARGE DE CONTACT
!
    lctfd = cfdisl(ds_contact%sdcont_defi,'FROT_DISCRET')
    lfrot = cfdisl(ds_contact%sdcont_defi,'FROTTEMENT')
!
    apcoef = ds_contact%sdcont_solv(1:14)//'.APCOEF'
    jeusup = ds_contact%sdcont_solv(1:14)//'.JSUPCO'
    tacfin = ds_contact%sdcont_solv(1:14)//'.TACFIN'
    tangco = ds_contact%sdcont_solv(1:14)//'.TANGCO'
    numlia = ds_contact%sdcont_solv(1:14)//'.NUMLIA'
    appoin = ds_contact%sdcont_solv(1:14)//'.APPOIN'
    nbddl = ds_contact%sdcont_solv(1:14)//'.NBDDL'
    apddl = ds_contact%sdcont_solv(1:14)//'.APDDL'
    jeuite = ds_contact%sdcont_solv(1:14)//'.JEUITE'
!
    call jeveuo(apcoef, 'L', japcoe)
    if (lctfd) then
        apcofr = ds_contact%sdcont_solv(1:14)//'.APCOFR'
        call jeveuo(apcofr, 'L', japcof)
    endif
    call jeveuo(jeusup, 'L', jjeusu)
    call jeveuo(tacfin, 'L', jtacf)
    call jeveuo(tangco, 'L', jtango)
    call jeveuo(numlia, 'L', jnumli)
    call jeveuo(appoin, 'L', japptr)
    call jeveuo(nbddl, 'L', jnbddl)
    call jeveuo(apddl, 'L', japddl)
    call jeveuo(jeuite, 'L', jjeuit)
!
    ztacf = cfmmvd('ZTACF')
!
! --- SD APPARIEMENT
!
    sdappa = ds_contact%sdcont_solv(1:14)//'.APPA'
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(ds_contact%sdcont_defi,'NDIM')
    nzoco = cfdisi(ds_contact%sdcont_defi,'NZOCO')
    ntnoe = cfdisi(ds_contact%sdcont_defi,'NTNOE')
    nbliai = cfdisd(ds_contact%sdcont_solv,'NBLIAI')
    nnoco = cfdisi(ds_contact%sdcont_defi,'NNOCO')
!
    write(ifm,*) '<CONTACT><APPA> RESULTATS DE L''APPARIEMENT'
!
! ----------------------------------------------------------------------
! --- INFOS SUR LES ZONES DE CONTACT
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ------ ZONES ------ '
!
    write(ifm,100) nzoco
    write(ifm,101) ntnoe
    write(ifm,102) nbliai
!
100 format (' <CONTACT><APPA> NOMBRE DE ZONES DE CONTACT        : ', i6)
101 format (' <CONTACT><APPA> NOMBRE MAXIMAL DE NOEUDS ESCLAVES : ', i6)
102 format (' <CONTACT><APPA> NOMBRE EFFECTIF DE LIAISONS       : ', i6)
!
! ----------------------------------------------------------------------
! --- INFOS SUR TOUS LES NOEUDS
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ------ NOEUDS DE CONTACT ------ '
!
    do ino = 1, nnoco
!
! ----- TYPE DU NOEUD
!
        posno = ino
        call cftypn(ds_contact%sdcont_defi, posno, typno)
!
! ----- NOM DU NOEUD
!
        call cfnomm(mesh, ds_contact%sdcont_defi, 'NOEU', posno, nomno)
!
        write(ifm,300) ino,nomno,typno
!
! ----- ZONE
!
        call cfzonn(ds_contact%sdcont_defi, posno, izone)
!
! ----- RECUPERATIONS DES TANGENTES AU NOEUD
!
        call apvect(sdappa, 'APPARI_NOEUD_TAU1', posno, tau1)
        call apvect(sdappa, 'APPARI_NOEUD_TAU2', posno, tau2)
!
! ----- NORMALE
!
        if (typno .eq. 'MAIT') then
            if (cfcald(ds_contact%sdcont_defi,izone ,'MAIT')) then
                write(ifm,303) (tau1(k),k=1,3)
                if (ndimg .eq. 3) then
                    write(ifm,305) (tau2(k),k=1,3)
                endif
                call mmnorm(ndimg, tau1, tau2, norm)
                write(ifm,306) (norm(k),k=1,3)
            else
                write(ifm,307)
            endif
        else
            if (cfcald(ds_contact%sdcont_defi,izone ,'ESCL')) then
                write(ifm,303) (tau1(k),k=1,3)
                if (ndimg .eq. 3) then
                    write(ifm,305) (tau2(k),k=1,3)
                endif
                call mmnorm(ndimg, tau1, tau2, norm)
                write(ifm,306) (norm(k),k=1,3)
            else
                write(ifm,307)
            endif
        endif
!
!
300 format (' <CONTACT><APPA> NOEUD NUMERO ',i6,' (',a8,') -> NOEUD ',a4)
303 format (' <CONTACT><APPA>  * TANGENTE 1  : ',3(1pe15.8,2x))
305 format (' <CONTACT><APPA>  * TANGENTE 2  : ',3(1pe15.8,2x))
306 format (' <CONTACT><APPA>  * NORMALE     : ',3(1pe15.8,2x))
307 format (' <CONTACT><APPA>  * TANGENTE ET NORMALE NON CALCULEES')
!
!
    end do
!
! ----------------------------------------------------------------------
! --- INFOS SUR LES NOEUDS ESCLAVES
! ----------------------------------------------------------------------
!
    write(ifm,*) '<CONTACT><APPA> ----- NOEUDS ESCLAVES ----- '
!
    do iliai = 1, nbliai
!
! ----- POINT DE CONTACT
!
        ip = zi(jnumli+4*(iliai-1)+1-1)
!
! ----- NOEUD ESCLAVE
!
        posnoe = zi(jnumli+4*(iliai-1)+2-1)
!
! ----- INFOS APPARIEMENT
!
        call apinfi(sdappa, 'APPARI_TYPE', ip, typapp)
        call apinfi(sdappa, 'APPARI_ENTITE', ip, entapp)
        call apinfi(sdappa, 'APPARI_ZONE', ip, izone)
!
! ----- NOM DU NOEUD ESCLAVE
!
        call apnomp(sdappa, ip, nompt)
        nomnoe = nompt(9:16)
        write(ifm,400) iliai,nompt
!
! ----- NOM ET TYPE DU MAITRE
!
        call cfnoap(mesh, ds_contact%sdcont_defi, typapp, entapp, nomapp,&
                    type2)
        if (typapp .lt. 0) then
            write(ifm,403)
            lnodal = .false.
        else if (typapp.eq.1) then
            write(ifm,401) nomapp
            nomnom = nomapp
            lnodal = .true.
        else if (typapp.eq.2) then
            write(ifm,402) nomapp
            nommam = nomapp
            lnodal = .false.
        else
            ASSERT(.false.)
        endif
!
! --- NOMBRE DE DDLS TOTAL: NBDDLT
!
        nbddlt = zi(japptr+iliai) - zi(japptr+iliai-1)
        nbddle = zi(jnbddl+posnoe) - zi(jnbddl+posnoe-1)
!
! --- AFFICHAGES
!
        write(ifm,404) nbddlt,nbddle
!
! ----- TANGENTES ET NORMALE
!
        tau1(1) = zr(jtango+6*(iliai-1)+1-1)
        tau1(2) = zr(jtango+6*(iliai-1)+2-1)
        tau1(3) = zr(jtango+6*(iliai-1)+3-1)
        tau2(1) = zr(jtango+6*(iliai-1)+4-1)
        tau2(2) = zr(jtango+6*(iliai-1)+5-1)
        tau2(3) = zr(jtango+6*(iliai-1)+6-1)
        call mmnorm(ndimg, tau1, tau2, norm)
        write(ifm,407) (norm(k),k=1,3)
        write(ifm,408) (tau1(k),k=1,3)
        if (ndimg .eq. 3) then
            write(ifm,508) (tau2(k),k=1,3)
        endif
!
! ----- JEUX
!
        jeuold = zr(jjeuit+3*(iliai-1)+1-1)
        dissup = zr(jjeusu+iliai-1)
        write(ifm,406) jeuold,dissup
!
! ----- PARAMETRES PENALISATION ET FROTTEMENT
!
        coefff = zr(jtacf+ztacf*(iliai-1)+0)
        coefpn = zr(jtacf+ztacf*(iliai-1)+1)
        coefpt = zr(jtacf+ztacf*(iliai-1)+2)
        write(ifm,700) coefpn
        write(ifm,701) coefpt
        write(ifm,703) coefff
!
! ----- DDL ET COEF CONTACT POUR NOEUD ESCLAVE
!
        jdece = zi(japptr+iliai-1)
        if (ndimg .eq. 3) then
            write (ifm,415) nomnoe, (zi(japddl+jdece+iddle-1),iddle= 1,nbddle),&
                                    (zr(japcoe+jdece+iddle-1),iddle= 1,nbddle)
        else
            write (ifm,418) nomnoe, (zi(japddl+jdece+iddle-1),iddle= 1,nbddle),&
                                    (zr(japcoe+jdece+iddle-1),iddle= 1,nbddle)
        endif
!
! ----- DDL ET COEF FROTTEMENT POUR NOEUD ESCLAVE
!
        jdece = zi(japptr+iliai-1)
        if (lfrot) then
            if (ndimg .eq. 3) then
                write (ifm,416) nomnoe, (zi(japddl+jdece+iddle-1), iddle=1,nbddle),&
                                        (zr(japcof+jdece+iddle-1), iddle=1,nbddle)
                write (ifm,417) nomnoe, (zi(japddl+jdece+iddle-1), iddle=1,nbddle),&
                                        (zr(japcof+30*ntnoe+jdece+iddle-1), iddle=1,nbddle)
            else
                write (ifm,419) nomnoe, (zi(japddl+jdece+iddle-1), iddle=1,nbddle),&
                                        (zr(japcof+jdece+iddle-1), iddle=1, nbddle)
            endif
        endif
!
! ----------------------------------------------------------------------
! --- DDL ET COEF POUR NOEUDS MAITRES
! ----------------------------------------------------------------------
!
        jdecm = jdece + nbddle
!
! ----- APPARIEMENT NODAL
!
        if (lnodal) then
            nbnom = 1
            inom = 1
            nbddlm = nbddlt - nbddle
!
! ------- COEFFICIENTS POUR CONTACT
!
            if (ndimg .eq. 3) then
                write (ifm,511) nomnom, (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                                        (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
            else
                write (ifm,611) nomnom, (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                                        (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
            endif
!
! ------- COEFFICIENTS POUR FROTTEMENT
!
            if (lfrot) then
                if (ndimg .eq. 3) then
                    write (ifm,512) nomnom,&
                      (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                      (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                    write (ifm,513) nomnom,&
                      (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                      (zr(japcof+30*ntnoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1, nbddlm)
                else
                    write (ifm,612) nomnom,&
                     (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                     (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                endif
            endif
!
! ----- APPARIEMENT MAITRE/ESCLAVE
!
        else
            posmam = entapp
            call cfnben(ds_contact%sdcont_defi, posmam, 'CONNEX', nbnom, jdecno)
            do inom = 1, nbnom
                call cfconn(ds_contact%sdcont_defi, jdecno, inom, posno2)
                nbddlm = zi(jnbddl+posno2) - zi(jnbddl+posno2-1)
!
                call cfnomm(mesh, ds_contact%sdcont_defi, 'NOEU', posno2, nomno2)
!
! --- COEFFICIENTS POUR CONTACT
!
                if (ndimg .eq. 3) then
                    write (ifm,501) nommam,nomno2,&
                        (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                        (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                else
                    write (ifm,601) nommam,nomno2,&
                        (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                        (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                endif
!
! --- COEFFICIENTS POUR FROTTEMENT
!
                if (lfrot) then
                    if (ndimg .eq. 3) then
                        write (ifm,502) nommam,nomno2,&
                            (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                            (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                        write (ifm,503) nommam,nomno2,&
                            (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm),&
                            (zr(japcof+30*ntnoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                    else
                        write (ifm,602) nommam,nomno2,&
                            (zi(japddl+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1, nbddlm),&
                            (zr(japcoe+jdecm+(inom-1)*nbddlm+iddlm-1), iddlm=1,nbddlm)
                    endif
                endif
            end do
        endif
    end do
!
400 format (' <CONTACT><APPA> LIAISON NUMERO ',i6,' (',a16,')')
401 format (' <CONTACT><APPA>  * APPARIEMENT AVEC NOEUD  ',a8)
402 format (' <CONTACT><APPA>  * APPARIEMENT AVEC MAILLE ',a8)
403 format (' <CONTACT><APPA>  * NON APPARIE')
404 format (' <CONTACT><APPA>  * NOMBRE DE DDLS : ',i6,' DONT ',i6,' POUR NOEUD ESCLAVE',i6)
406 format (' <CONTACT><APPA>  * JEU: ',1pe15.8,' DONT :',1pe15.8, ' VENANT DE DIST_*')
407 format (' <CONTACT><APPA>  * NORMALE LISSEE/MOYENNEE: ', 3(1pe15.8,2x))
408 format (' <CONTACT><APPA>  * TANGENTE DIRECTION 1   : ', 3(1pe15.8,2x))
508 format (' <CONTACT><APPA>  * TANGENTE DIRECTION 2   : ', 3(1pe15.8,2x))
!
! --- 3D - ESCLAVE
!
415 format ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
416 format ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
417 format ((' <CONTACT><APPA>  * DDL ESCL. FROT2   ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
!
! --- 2D - ESCLAVE
!
418 format ((' <CONTACT><APPA>  * DDL ESCL. CONTACT ( ',a8,'):', 2(i8,2x),' / ', 2(1pe15.8,2x)))
419 format ((' <CONTACT><APPA>  * DDL ESCL. FROT1   ( ',a8,'):', 2(i8,2x),' / ', 2(1pe15.8,2x)))
!
! --- 3D MAITRE/ESCL - MAITRE
!
501 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8, '/',a8,'):', 3(i8,2x),' / ',&
             3(1pe15.8,2x)))
502 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8, '/',a8,'):', 3(i8,2x),' / ',&
             3(1pe15.8,2x)))
503 format ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',a8, '/',a8,'):', 3(i8,2x),' / ',&
             3(1pe15.8,2x)))
!
! --- 3D NODAL - MAITRE
!
511 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
512 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
513 format ((' <CONTACT><APPA>  * DDL MAIT. FROT2   ( ',a8,'):', 3(i8,2x),' / ', 3(1pe15.8,2x)))
!
! --- 2D MAITRE/ESCL - MAITRE
!
601 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8, '/',a8,'):', 2(i8,2x),' / ',&
           2(1pe15.8,2x)))
602 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8, '/',a8,'):', 2(i8,2x),' / ',&
            2(1pe15.8,2x)))
!
! --- 2D NODAL - MAITRE
!
611 format ((' <CONTACT><APPA>  * DDL MAIT. CONTACT ( ',a8,'):', 2(i8,2x),' / ', 2(1pe15.8,2x)))
612 format ((' <CONTACT><APPA>  * DDL MAIT. FROT1   ( ',a8,'):', 2(i8,2x),' / ', 2(1pe15.8,2x)))
!
700 format (' <CONTACT><APPA>  * E_N              :',1pe15.8)
701 format (' <CONTACT><APPA>  * E_T              :',1pe15.8)
703 format (' <CONTACT><APPA>  * COULOMB          :',1pe15.8)
!
    call jedema()
!
end subroutine
