   M S B A M L   `   `   `  ����B  >Reversi, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null!clr-namespace:ReversiReversi  5          Reversi.App   5      W SPresentationCore, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35R NWindowsBase, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35\ XPresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35D 9http://schemas.microsoft.com/winfx/2006/xaml/presentation    5      R NSystem.Xaml, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml     5      'localclr-namespace:Reversi     5        �� 	Resources  5         LightPrimaryColor  BackgroundLinearGradient �� EndPoint � MappingMode �� 
StartPoint � Color � Offset  PrimaryBrush �� Color  MedPrimaryBrush  LightPrimaryBrush  
BlackBrush  	GrayBrush  MedGrayBrush  LightGrayBrush 	 buttonStyle �� 
TargetType �� Property	 �� Value
 �� 	Resources �� Triggers :� RoutedEvent �� TargetProperty8  2System.Windows.Media.Animation.EasingColorKeyFrame �� KeyTime �� Value   EasingFunction4  .System.Windows.Media.Animation.ExponentialEase7  1System.Windows.Media.Animation.EasingFunctionBase   
EasingMode g� AutoReverse2  ,System.Windows.Media.Animation.QuadraticEase 
 EnterHeadingText �� 
TargetName O� KeyTime O� Value �� 	KeySpline  WhiteButton �� 
TargetType  WhiteIconButton �� Padding �� CornerRadius  GrowEllipse	 W� To g� Duration W� EasingFunction-  'System.Windows.Media.Animation.BackEase   	Amplitude  ShrinkEllipse  ShrinkBorderThickness	 t� To t� EasingFunction  GrowBorderThickness��%   &	        &	 !     7   &	 �     &	 �     &	     7   &	     &	 <    &	 _    &	 �    &		 �    &	
 x    &	 �    7 	 &	 H	    7 	 &	 �	    &	 �    &	 2    &	 �    ��5   
   	#FFFFF5F5��65   �� 5   
   $ 0.5,1&�6?   $ RelativeToBoundingBox=�6P   $	 0,0&�6t   .��5	      � $ White��6   � 5
      8   $ 1Q�6F   5      �� 5   
   $ 	#FF873545��6/   �� 5   
   $ 	#FFE23E5B��62   �� 5   
   8   64   �� 5   
   $ 	#E5000000��6-   �� 5   
   $ 	#B2000000��6,   �� 5   
   $ 	#FFD8D8D8��6/   �� 5   
   $ 	#FFF0F0F0��61   �� 5   
   " ��6$   .�5      ��  � ��
	 �����6,   �� 5       � ��$	 1q�60   �� 5       �@��CornerRadius$	 5��6-   �� 5       �@��Padding$	 20, 10q�6(   �� 5       � ��$	 Center=�64   �� 5       � ��$	 Center=�62   �� 5       � ��
	 ���� 6+   
 5      ���� 5      '��       " ��6   .�5      ��  � ��$	 14(�61   6>   �� 5       � ��$	 Center=�6<   6M   �� 5       � ��$	 Center=�6:   6K   �� 5        � ��
	 �����6C   5!      5"       5#      :� 5$      $ 
MouseEnter��6   [�5%      �� 5&      .��5'      �� .��5(   "   �� $0 *(Panel.Background).(SolidColorBrush.Color)�6?   .g�5)   &     $ 0:0:0.2��6:   $ 	#33FFFFFF��6L    5*   *     5+   .   $ EaseOut=�6>   5-   '   5.   #   5/      50      52      :� 53      $ 
MouseLeave��6   [�54      �� 55      .��56      �� .��57   "   �� $0 *(Panel.Background).(SolidColorBrush.Color)�6?   .g�58   &     $ 0:0:0.2��6:   $ 	#00FFFFFF��6L    59   *     5:   .   $ EaseOut=�6>   5<   '   5=   #   5>      5?      5A      :� 5B      $ MouseLeftButtonDown��6   [�5C      �� 5D      .��5E      ��  . .��5F   "   �� $0 *(Panel.Background).(SolidColorBrush.Color)�6?   .g�5G   &     $ 0:0:0.1��6:   $ 	#66FFFFFF��6L    5H   *     5I   .   $ EaseOut=�6<   5K   '   5L   #   5M      5N      5P      5R      �� 5S   
   .��5T      U� $U O(UIElement.RenderTransform).(TransformGroup.Children)[3].(TranslateTransform.X)�6,   $ reversiTextLabel��6�   .\�5U      �� $ 0��6'   $
 -363Q�63   �� 5V      $ 0:0:3��6'   $ 0Q�67   $ 0,1,0,1��6A   5W      5X      �� 5Y   
   " ��6.   .R�5Z      �� 8��  ._�5[      �� #��z��6   $��14(�5\      $��Center=�6*   $��Center=�6G   
�������$��Segoe UI Semibold)�6u   5]      5^      �� 5_   
   " ��62   .R�5`      �� 8��  $��0q�6:   $ 5q�6N   $ 0��6Z   ._�5a      �� #��z��6#   5b      5c      �� 5d   
   .��5e      W� $V P(UIElement.RenderTransform).(TransformGroup.Children)[0].(ScaleTransform.ScaleX)�6   $ 1X�5f      $ 0:0:0.4D�6%    5g        5h      $ EaseOut=�6   $	 0.5Q�64   5j      W� 5k      $V P(UIElement.RenderTransform).(TransformGroup.Children)[0].(ScaleTransform.ScaleY)�6   $ 1X�5l      $ 0:0:0.4D�6%    5m        5n      $ EaseOut=�6   $	 0.5Q�64   5p      5q      �� 5r   
   .��5s      W� $V P(UIElement.RenderTransform).(TransformGroup.Children)[0].(ScaleTransform.ScaleX)�6   $ 0X�5t      $ 0:0:0.2D�6%    5u        5v      $ EaseOut=�6&   5x      W� 5y      $V P(UIElement.RenderTransform).(TransformGroup.Children)[0].(ScaleTransform.ScaleY)�6   $ 0X�5z      $ 0:0:0.2D�6%    5{        5|      $ EaseOut=�6&   5~      5      �� 5�   
   .��5�      t� $ BorderThickness�6!   $ 0X�6M   $ 0:0:0.1D�6T    5�        5�      $ EaseOut=�6&   5�      5�      �� 5�   
   .��5�      t� $ BorderThickness�6!   $ 5X�6M   $ 0:0:0.2D�6T    5�        5�      $ EaseOut=�6&   5�      5�      5�      5�      