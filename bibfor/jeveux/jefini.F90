subroutine jefini(cond)
! person_in_charge: j-pierre.lefebvre at edf.fr
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
    implicit none
#include "jeveux_private.h"
#include "asterc/asabrt.h"
#include "asterc/xfini.h"
#include "asterfort/assert.h"
#include "asterfort/enlird.h"
#include "asterfort/iunifi.h"
#include "asterfort/jeimpm.h"
#include "asterfort/jeimpr.h"
#include "asterfort/jelibf.h"
#include "asterfort/jjlidy.h"
#include "asterfort/u2mesg.h"
#include "asterfort/ulclos.h"
#include "asterfort/utgtme.h"
    character(len=*) :: cond
!     ==================================================================
!-----------------------------------------------------------------------
    integer :: i, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
!
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: nbfic
    common /iparje/  nbfic
!     ------------------------------------------------------------------
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: icdyn, mxltot
    common /xdynje/  icdyn , mxltot
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
!     ==================================================================
    integer :: vali(7), info, ifm, ires, iret, ibid
    character(len=8) :: kcond, staou, k8tab(3)
    character(len=24) :: ladate
    real(kind=8) :: rval(3)
!     ------------------------------------------------------------------
!
    kcond = cond ( 1: min( len(cond) , len(kcond) ) )
    ASSERT(kcond .eq. 'NORMAL  ' .or. kcond .eq. 'ERREUR  ' .or. kcond .ne. 'TEST    ')
    if (kcond .eq. 'NORMAL  ' .or. kcond .eq. 'TEST    ') then
        staou = '        '
    else
        staou = 'SAUVE   '
    endif
!
!     EVALUATION DE LA CONSOMMATION MEMOIRE
!
    k8tab(1)='VMPEAK'
    k8tab(2)='MEM_TOTA'
    call utgtme(2, k8tab, rval, iret)
!
    if (rval(2) .lt. rval(1)) then
        call u2mesg('I', 'JEVEUX1_77', 0, ' ', 0,&
                    ibid, 2, rval)
    else if ((rval(2)-rval(1))/rval(1) .gt. 0.5d0) then
        call u2mesg('I', 'JEVEUX1_78', 0, ' ', 0,&
                    ibid, 2, rval)
    endif
!
!     -------------  EDITION DES REPERTOIRES ---------------------------
    if (kcond .eq. 'TEST    ') then
        do 5 i = 1, nbfic
            if (classe(i:i) .ne. ' ') then
                call jeimpr(6, classe(i:i), '     JEFINI     ' // kcond)
            endif
 5      continue
!     -------------  EDITION SEGMENTATION MEMOIRE ----------------------
        call jeimpm(6)
    endif
!     -------------  LIBERATION FICHIER --------------------------------
    if (kcond .ne. 'ERREUR  ') then
        info = 1
        do 10 i = 1, nbfic
            if (classe(i:i) .ne. ' ') then
                call jelibf(staou, classe(i:i), info)
            endif
10      continue
!       -----------  DESALLOCATION GESTION DES MARQUES -----------------
        call jjlidy(kdesma(2), kdesma(1))
        call jjlidy(kposma(2), kposma(1))
        kdesma(1) = 0
        kdesma(2) = 0
        kposma(1) = 0
        kposma(2) = 0
!
    else
        call asabrt(6)
    endif
!
!     --- IMPRESSION DES CONSOMMATIONS MEMOIRE ---
!
    k8tab(1) = 'CMXU_JV'
    k8tab(2) = 'CMAX_JV'
    k8tab(3) = 'VMPEAK'
    call utgtme(3, k8tab, rval, iret)
    ifm = iunifi('MESSAGE')
    ires = iunifi('RESULTAT')
!
    if (ires .gt. 0) then
        write(ires,*) ' '
        write(ires,'(2A,F11.2,A)')&
     &        ' <I> <FIN> MEMOIRE JEVEUX MINIMALE REQUISE POUR ',&
     &        'L''EXECUTION :                ',rval(1),' Mo'
        write(ires,'(2A,F11.2,A)')&
     &        ' <I> <FIN> MEMOIRE JEVEUX OPTIMALE REQUISE POUR ',&
     &        'L''EXECUTION :                ',rval(2),' Mo'
        if (rval(3) .gt. 0) then
            write(ires,'(2A,F11.2,A)')&
     &        ' <I> <FIN> MAXIMUM DE MEMOIRE UTILISEE PAR LE PROCESSUS'&
     &        ,' LORS DE L''EXECUTION :',rval(3),' Mo'
        endif
    endif
!
    if (kcond .ne. 'TEST    ') then
        if (ifm .gt. 0) then
            write(ifm,*) ' '
            write(ifm,*) '<I>       FERMETURE DES BASES EFFECTUEE'
            if (ldyn .eq. 1) then
                vali(1) = nint(mxdyn/(1024*1024))
                vali(2) = liszon*lois/(1024*1024)
                vali(3) = nbdyn
                vali(4) = nbfree
                vali(5) = nint(mldyn/(1024*1024))
                vali(6) = nint(lgio(1)/(1024*1024))
                vali(7) = nint(lgio(2)/(1024*1024))
                write(ifm,*) ' '
                write(ifm,*) '  STATISTIQUES CONCERNANT L'''&
     &                 //'ALLOCATION DYNAMIQUE :'
                write(ifm,*) '    TAILLE CUMULEE MAXIMUM            :',&
     &                 vali(1),' Mo.'
                write(ifm,*) '    TAILLE CUMULEE LIBEREE            :',&
     &                 vali(5),' Mo.'
                write(ifm,*) '    NOMBRE TOTAL D''ALLOCATIONS        :',&
     &                 vali(3)
                write(ifm,*) '    NOMBRE TOTAL DE LIBERATIONS       :',&
     &                 vali(4)
                write(ifm,*) '    APPELS AU MECANISME DE LIBERATION :',&
     &                 icdyn
                write(ifm,*) '    TAILLE MEMOIRE CUMULEE RECUPEREE  :',&
     &                 mxltot,' Mo.'
                write(ifm,*) '    VOLUME DES LECTURES               :',&
     &                  vali(6),' Mo.'
                write(ifm,*) '    VOLUME DES ECRITURES              :',&
     &                  vali(7),' Mo.'
                write(ifm,*) ' '
            endif
            write(ifm,'(A,F11.2,A)')&
     &       '   MEMOIRE JEVEUX MINIMALE REQUISE POUR L''EXECUTION :',&
     &       rval(1),' Mo'
            write(ifm,'(A)') '     - IMPOSE DE NOMBREUX ACCES DISQUE'
            write(ifm,'(A)') '     - RALENTIT LA VITESSE D''EXECUTION'
            write(ifm,'(A,F11.2,A)')&
     &       '   MEMOIRE JEVEUX OPTIMALE REQUISE POUR L''EXECUTION :',&
     &       rval(2),' Mo'
            write(ifm,'(A)') '     - LIMITE LES ACCES DISQUE'
            write(ifm,'(A)') '     - AMELIORE LA VITESSE D''EXECUTION'
            if (rval(3) .gt. 0) then
                write(ifm,'(A,F11.2,A)')&
     &       '   MAXIMUM DE MEMOIRE UTILISEE PAR LE PROCESSUS     :',&
     &       rval(3),' Mo'
                write(ifm,'(A)') '     - COMPREND LA MEMOIRE CONSOMMEE PAR '//&
     &       ' JEVEUX, '
                write(ifm,'(A)') '       LE SUPERVISEUR PYTHON, '//&
     &       'LES LIBRAIRIES EXTERNES'
            endif
            write(ifm,*) ' '
!
            call enlird(ladate)
            write(ifm,*) '<I>       FIN D''EXECUTION LE : '//ladate
!
        endif
!
!       --- ON FERME TOUT ---
        call ulclos()
!
        call xfini(19)
    endif
end subroutine
